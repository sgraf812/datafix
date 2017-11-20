{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This whole module is basically a huge hack that
-- compiles a haskell module and returns the expression
-- bound to the top-level, non-recursive `expr` binding.
module Analyses.Syntax.MkCoreFromFile where

import           Control.Monad.IO.Class
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (First (..))
import qualified Data.Text                          as Text
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Toolkit        (getGHCPackageDBFlags,
                                                     localBuildInfoQ)
import           Prelude                            hiding (FilePath)
import qualified Prelude
import           System.Directory                   (canonicalizePath)
import           System.FilePath                    (splitSearchPath)
import           Turtle

import           CoreSyn
import           CoreTidy                           (tidyExpr)
import           DynFlags
import           GHC
import qualified GHC.Paths
import           Id
import           Name
import           Outputable
import           Packages
import           VarEnv                             (emptyTidyEnv)

findTopLevelDecl :: String -> CoreProgram -> Maybe CoreExpr
findTopLevelDecl occ = getFirst . foldMap (First . findName)
  where
    findName (NonRec id_ rhs)
      | occNameString (occName (idName id_)) == occ
      = Just rhs
    findName _ = Nothing


compileCoreExpr :: Prelude.FilePath -> IO CoreExpr
compileCoreExpr modulePath = runGhc (Just GHC.Paths.libdir) $ do
  -- Don't generate any artifacts
  _ <- getSessionDynFlags >>= setSessionDynFlags . (\df -> df { hscTarget = HscNothing })
  -- Set up the package database

  addPkgDbs $(localBuildInfoQ)
  m <- liftIO (canonicalizePath modulePath) >>= compileToCoreModule
  pure $
    tidyExpr emptyTidyEnv
    -- . pprTraceIt "expr"
    . fromMaybe (error "Could not find top-level non-recursive binding `expr`")
    . findTopLevelDecl "expr"
    . cm_binds
    $ m


stackPkgDbs :: MonadIO m => m (Maybe [Prelude.FilePath])
stackPkgDbs = do
  (ec, paths) <- procStrict "stack" ["path", "--ghc-package-path"] mempty
  if ec == ExitSuccess
    then pure (Just (splitSearchPath (Text.unpack paths)))
    else pure Nothing


-- | Add a list of package databases to the Ghc monad.
addPkgDbs :: (MonadIO m, GhcMonad m) => LocalBuildInfo -> m ()
addPkgDbs lbi = do
  dfs <- getSessionDynFlags
  let pkgs = getGHCPackageDBFlags lbi
#if MIN_VERSION_Cabal(2,0,0)
  let dfs' = dfs { packageDBFlags = pkgs }
#else
  let dfs' = dfs { extraPkgConfs = (pkgs ++) . extraPkgConfs dfs }
#endif
  _ <- setSessionDynFlags dfs'
  _ <- liftIO $ initPackages dfs'
  return ()
