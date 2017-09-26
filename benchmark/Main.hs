import           Algebra.Lattice
import           Criterion
import           Criterion.Main
import           Datafix
import           Datafix.Worklist       (Density (..), IterationBound (..),
                                         fixProblem)
import           Datafix.Worklist.Graph (GraphRef)
import           Numeric.Natural

import           Sum

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

fixSum :: GraphRef graph => (Node -> Density graph) -> Int -> Natural
fixSum density n = fixProblem sumProblem (density (Node n)) NeverAbort (Node n)

main :: IO ()
main = defaultMain
  [ bgroup "sum" [ sumGroup 100, sumGroup 1000, sumGroup 10000 ]
  ] where
      sumGroup n =
        bgroup (show n)
          [ bench "baseline" (whnf (\n -> sum [1..n]) n)
          , bench "sparse" (whnf (fixSum (const Sparse)) n)
          , bench "dense"  (whnf (fixSum Dense) n)
          ]
