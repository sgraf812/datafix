{-# OPTIONS_GHC -fno-warn-orphans #-}

module Critical (tests) where

import           Algebra.Lattice
import           Data.Proxy
import           Datafix
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.HUnit

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

tests :: [TestTree]
tests =
  [ testGroup "One node with loop"
      [ testCase "stabilises at 10" (fixProblem loopProblem (GraphNode 0) @?= 10)
      ]
  , testGroup "One node with double dependency on node with loop"
      [ testCase "stabilizes at 4" (fixProblem doubleDependencyProblem (GraphNode 0) @?= 4)
      ]
  ]

p :: Proxy Natural
p = Proxy

mkDFP :: (GraphNode -> TransferFunction (DependencyM Natural) Natural) -> DataFlowProblem Natural
mkDFP transfer = DFP transfer (const (eqChangeDetector p))

-- | One node graph with loop that stabilizes after 10 iterations.
loopProblem :: DataFlowProblem Natural
loopProblem = mkDFP transfer
  where
    transfer (GraphNode 0) = do -- stabilizes at 10
      n <- dependOn p (GraphNode 0)
      return (min (n + 1) 10)

-- | Two node graph (nodes @A@, @B@), where @A@ `dependOn` @B@ twice and @B@
-- has a loop. 
-- 
-- The idea here is that the second change of @B@ from 1 to 2 makes @A@
-- unstable, so that it gets iterated again, which results in a value of
-- 4 instead of e.g. 3 (= 1 + 2, the values of @B@ in the first iteration
-- of @A@).
doubleDependencyProblem :: DataFlowProblem Natural
doubleDependencyProblem = mkDFP transfer  
  where
    transfer (GraphNode 0) = do -- stabilizes at 4
      n <- dependOn p (GraphNode 1)
      m <- dependOn p (GraphNode 1)
      return (n + m)
    transfer (GraphNode 1) = do -- stabilizes at 2
      n <- dependOn p (GraphNode 1)
      return (min (n + 1) 2)
