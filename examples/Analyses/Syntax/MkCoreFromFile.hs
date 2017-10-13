{-# LANGUAGE OverloadedStrings #-}

-- | This whole module is basically a huge hack that
-- compiles a haskell module and returns the expression
-- bound to the top-level, non-recursive `expr` binding.
module Analyses.Syntax.MkCoreFromFile where

import           Control.Monad.IO.Class
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (First (..))
import qualified Data.Text              as Text
import           Prelude                hiding (FilePath)
import qualified Prelude
import           System.Directory       (canonicalizePath)
import           System.FilePath        (splitSearchPath)
import           Turtle

import           CoreSyn
import           CoreTidy               (tidyExpr)
import           DynFlags
import           GHC
import qualified GHC.Paths
import           Id
import           Name
import           Outputable
import           Packages
import           VarEnv                 (emptyTidyEnv)


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
  Just dbs <- stackPkgDbs
  addPkgDbs dbs
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


-- | Add a package database to the Ghc monad
addPkgDb :: (MonadIO m, GhcMonad m) => Prelude.FilePath -> m ()
addPkgDb db = addPkgDbs [db]


-- | Add a list of package databases to the Ghc monad
-- This should be equivalen to
-- > addPkgDbs ls = mapM_ addPkgDb ls
-- but it is actaully faster, because it does the package
-- reintialization after adding all the databases
addPkgDbs :: (MonadIO m, GhcMonad m) => [Prelude.FilePath] -> m ()
addPkgDbs fps = do
  dfs <- getSessionDynFlags
  let pkgs = map PkgConfFile fps
  let dfs' = dfs { extraPkgConfs = (pkgs ++) . extraPkgConfs dfs }
  _ <- setSessionDynFlags dfs'
  _ <- liftIO $ initPackages dfs'
  return ()
