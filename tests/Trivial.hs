{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Trivial (tests) where

import           Datafix.SemiLattice
import           Datafix
import           Datafix.Worklist.Graph (GraphRef)
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.HUnit

import           Fac
import           Fib
import           Mutual

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

fixFib, fixFac, fixMutualRecursive
  :: GraphRef graph => (Node -> Density graph) -> Int -> Natural
fixFib density n = solveProblem fibFramework (density (Node n)) NeverAbort (dependOn @(DependencyM _ Natural) (Node n))
fixFac density n = solveProblem facFramework (density (Node n)) NeverAbort (dependOn @(DependencyM _ Natural) (Node n))
fixMutualRecursive density n = solveProblem mutualRecursiveFramework (density (Node 1)) NeverAbort (dependOn @(DependencyM _ Natural) (Node n))

tests :: [TestTree]
tests =
  [ testGroup "Memoization"
      [ testGroup "Sparse"
          [ testCase "fibonacci 10" (fixFib (const Sparse) 10 @?= fib 10)
          , testCase "factorial 100" (fixFac (const Sparse) 100 @?= fac 100)
          ]
      , testGroup "Dense"
          [ testCase "fibonacci 10" (fixFib Dense 10 @?= fib 10)
          , testCase "factorial 100" (fixFac Dense 100 @?= fac 100)
          ]
      ]
  , testGroup "mutual recursion"
      [ testGroup "Sparse"
          [ testCase "first node is stable" (fixMutualRecursive (const Sparse) 0 @?= 11)
          , testCase "second node is stable" (fixMutualRecursive (const Sparse) 1 @?= 10)
          ]
      , testGroup "Dense"
          [ testCase "first node is stable" (fixMutualRecursive Dense 0 @?= 11)
          , testCase "second node is stable" (fixMutualRecursive Dense 1 @?= 10)
          ]
      , testGroup "Abortion"
          [ testCase "aborts after 5 updates with value 42" (solveProblem mutualRecursiveFramework Sparse (AbortAfter 5 (const 42)) (dependOn @(DependencyM _ Natural) (Node 1)) @?= 42)
          ]
      ]
  ]
