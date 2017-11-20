{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Algebra.Lattice
import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import           Datafix
import           Datafix.Worklist               (Density (..),
                                                 IterationBound (..),
                                                 fixProblem)
import           Datafix.Worklist.Graph         (GraphRef)
import           Numeric.Natural

import qualified Analyses.AdHocStrAnal          as AdHocStrAnal
import qualified Analyses.StrAnal               as StrAnal
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.MkCoreFromFile (compileCoreExpr)
import           Analyses.Syntax.MkCoreHelpers
import           Sum

import           CoreSeq                        (seqExpr)
import           CoreSyn
import           CoreTidy                       (tidyExpr)
import           Id
import           VarEnv                         (emptyTidyEnv)

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

-- | For 'Criterion.env'.
instance NFData CoreExpr where
  rnf = seqExpr

fixSum :: GraphRef graph => (Node -> Density graph) -> Int -> Natural
fixSum density n = fixProblem sumProblem (density (Node n)) NeverAbort (Node n)

main :: IO ()
main = defaultMain
  [ bgroup "sum" $ map sumGroup [100, 1000, 10000]
  , bgroup "stranal"
      [ strAnalGroup "simpleRecursive1" simpleRecursive1
      , strAnalGroup "nestedRecursive1" nestedRecursive1
      , strAnalFileGroup "exprs/const.hs"
      , strAnalFileGroup "exprs/findLT.hs"
      , strAnalFileGroup "exprs/kahan.hs"
      , strAnalFileGroup "exprs/sieve.hs"
      , strAnalFileGroup "exprs/lambda.hs"
      ]
  ] where
      sumGroup n =
        bgroup (show n)
          [ bench "baseline" (whnf (\n' -> sum [1..n']) n)
          , bench "sparse"   (whnf (fixSum (const Sparse)) n)
          , bench "dense"    (whnf (fixSum Dense) n)
          ]
      strAnalGroup descr e =
        bgroup descr
          [ bench "baseline" (whnf (seqStrLattice . AdHocStrAnal.analyse) e)
          , bench "sparse"   (whnf (seqStrLattice . StrAnal.analyse) e)
          , bench "dense"    (whnf (seqStrLattice . StrAnal.analyseDense) e)
          ]
      strAnalFileGroup file =
        env (compileCoreExpr file) $ \e ->
          bgroup file
            [ bench "baseline" (whnf (seqStrLattice . AdHocStrAnal.analyse) e)
            , bench "sparse"   (whnf (seqStrLattice . StrAnal.analyse) e)
            , bench "dense"    (whnf (seqStrLattice . StrAnal.analyseDense) e)
            ]


seqStrLattice :: StrLattice -> ()
seqStrLattice l = strType l `seq` annotations l `seq` ()

x, x1, x2, y, z, b, b1, b2, f, g :: Id
[x, x1, x2, y, z, b, b1, b2, f, g] = mkTestIds
  [ ("x", int)
  , ("x1", int)
  , ("x2", int)
  , ("y", int)
  , ("z", int)
  , ("b", bool)
  , ("b1", bool)
  , ("b2", bool)
  , ("f", bool2int2int)
  , ("g", bool2int2int)
  ]


-- | @
-- let f b x =
--       if b
--         then f b z
--         else z
-- in f False 1
-- @
simpleRecursive1 :: CoreExpr
simpleRecursive1 = tidyExpr emptyTidyEnv $
  letrec
    f (lam b $ lam x $
        ite (var b)
          (var f $$ var b $$ var z)
          (var z))
    (var f $$ boolLit False $$ intLit 1)


-- | @
-- let f b1 x1 =
--       let g b2 x2 =
--             if b2
--               then g b2 z
--               else f b2 x2
--       in if b1
--            then g b1 x1
--            else z
-- in f False 1
-- @
nestedRecursive1 :: CoreExpr
nestedRecursive1 = tidyExpr emptyTidyEnv $
  letrec
    f (lam b1 $ lam x1 $
        letrec
          g (lam b2 $ lam x2 $
              ite (var b2)
                (var g $$ var b2 $$ var z)
                (var f $$ var b2 $$ var x2))
          (ite (var b)
            (var g $$ var b1 $$ var x1)
            (var z)))
    (var f $$ boolLit False $$ intLit 1)
