{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Critical (tests) where

import           Datafix.Utils.SemiLattice
import           Datafix
import           Datafix.Worklist.Graph (GraphRef)
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.HUnit

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

fixLoop, fixDoubleDependency
  :: GraphRef graph => (Node -> Density graph) -> Int -> Natural
fixLoop density n = solveProblem loopFramework (density (Node 0)) NeverAbort (dependOn @(DependencyM _ Natural) (Node n))
fixDoubleDependency density n = solveProblem doubleDependencyFramework (density (Node 1)) NeverAbort (dependOn @(DependencyM _ Natural) (Node n))

tests :: [TestTree]
tests =
  [ testGroup "One node with loop"
      [ testGroup "Sparse"
          [ testCase "stabilises at 10" (fixLoop (const Sparse) 0 @?= 10)
          ]
      , testGroup "Dense"
          [ testCase "stabilises at 10" (fixLoop Dense 0 @?= 10)
          ]
      ]
  , testGroup "One node with double dependency on node with loop"
      [ testGroup "Sparse"
          [ testCase "stabilizes at 4" (fixDoubleDependency (const Sparse) 0 @?= 4)
          ]
      , testGroup "Dense"
          [ testCase "stabilizes at 4" (fixDoubleDependency Dense 0 @?= 4)
          ]
      , testGroup "Abortion"
          [ testCase "stabilizes at or over 4" (assertBool ">= 4" $ solveProblem doubleDependencyFramework Sparse (AbortAfter 1 (+ 4)) (dependOn @(DependencyM _ Natural) (Node 0)) >= 4)
          ]
      ]
  ]

mkDFF :: forall m . (Domain m ~ Natural) => (Node -> LiftedFunc Natural m) -> DataFlowFramework m
mkDFF transfer = DFF transfer (const (eqChangeDetector @(Domain m)))

-- | One node graph with loop that stabilizes after 10 iterations.
loopFramework :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowFramework m
loopFramework = mkDFF transfer
  where
    transfer (Node 0) = do -- stabilizes at 10
      n <- dependOn @m (Node 0)
      return (min (n + 1) 10)
    transfer (Node _) = error "Invalid node"

-- | Two node graph (nodes @A@, @B@), where @A@ `dependOn` @B@ twice and @B@
-- has a loop.
--
-- The idea here is that the second change of @B@ from 1 to 2 makes @A@
-- unstable, so that it gets iterated again, which results in a value of
-- 4 instead of e.g. 3 (= 1 + 2, the values of @B@ in the first iteration
-- of @A@).
doubleDependencyFramework :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowFramework m
doubleDependencyFramework = mkDFF transfer
  where
    transfer (Node 0) = do -- stabilizes at 4
      n <- dependOn @m (Node 1)
      m <- dependOn @m (Node 1)
      return (n + m)
    transfer (Node 1) = do -- stabilizes at 2
      n <- dependOn @m (Node 1)
      return (min (n + 1) 2)
    transfer (Node _) = error "Invalid node"
