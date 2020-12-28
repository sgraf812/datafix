{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

import           System.Environment
import qualified Analyses.AdHocStrAnal          as AdHocStrAnal
import qualified Analyses.StrAnal               as StrAnal
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.MkCoreFromFile (compileCoreExpr)
import           Control.Exception

seqStrLattice :: StrLattice -> ()
seqStrLattice l = strTy l `seq` strAnns l `seq` ()

main :: IO ()
main = do
  (variant:file:_) <- getArgs
  e <- compileCoreExpr file
  evaluate $ seqStrLattice $ case variant of
    "baseline" -> AdHocStrAnal.analyse e
    "datafix"  -> StrAnal.analyse e
    _ -> error "variant must be either baseline or datafix"
