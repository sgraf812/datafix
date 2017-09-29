{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Algebra.Lattice
import           Criterion
import           Criterion.Main
import           Datafix
import           Datafix.Worklist              (Density (..),
                                                IterationBound (..), fixProblem)
import           Datafix.Worklist.Graph        (GraphRef)
import           Numeric.Natural

import qualified Analyses.AdHocStrAnal         as AdHocStrAnal
import qualified Analyses.StrAnal              as StrAnal
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.MkCoreHelpers
import           Sum

import           CoreSyn
import           CoreTidy                      (tidyExpr)
import           Id
import           VarEnv                        (emptyTidyEnv)

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

fixSum :: GraphRef graph => (Node -> Density graph) -> Int -> Natural
fixSum density n = fixProblem sumProblem (density (Node n)) NeverAbort (Node n)

main :: IO ()
main = defaultMain
  [ bgroup "sum" $ map sumGroup [100, 1000, 10000]
  , bgroup "stranal" $ map (uncurry strAnalGroup)
      [ ("simpleRecursive1", simpleRecursive1)
      , ("simpleRecursive2", simpleRecursive2)
      ]
  ] where
      strAnalGroup descr e =
        bgroup descr
          [ bench "baseline" (whnf (seqStrLattice . AdHocStrAnal.analyse) e)
          , bench "sparse"   (whnf (seqStrLattice . StrAnal.analyse) e)
          , bench "dense"    (whnf (seqStrLattice . StrAnal.analyseDense 2) e)
          ]
      sumGroup n =
        bgroup (show n)
          [ bench "baseline" (whnf (\n' -> sum [1..n']) n)
          , bench "sparse"   (whnf (fixSum (const Sparse)) n)
          , bench "dense"    (whnf (fixSum Dense) n)
          ]

seqStrLattice :: StrLattice -> ()
seqStrLattice l = strType l `seq` annotations l `seq` ()

x, y, z, b, f :: Id
[x, y, z, b, f] = mkTestIds
  [ ("x", int)
  , ("y", int)
  , ("z", int)
  , ("b", bool)
  , ("f", bool2int2int)
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
-- let f b =
--       if b
--         then \x -> f b z
--         else \y -> z
-- in f False 1
-- @
simpleRecursive2 :: CoreExpr
simpleRecursive2 = tidyExpr emptyTidyEnv $
  letrec
    f (lam b $
        ite (var b)
          (lam x (var f $$ var b $$ var z))
          (lam y (var z)))
    (var f $$ boolLit False $$ intLit 1)
