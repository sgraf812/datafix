{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Trivial (tests) where

import           Algebra.Lattice
import           Data.Proxy
import           Datafix
import           Datafix.Worklist (Density (..), fixProblem)
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.HUnit

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

fixFib density n = fixProblem fibProblem (density (Node n)) (Node n)
fixFac density n = fixProblem facProblem (density (Node n)) (Node n)
fixMutualRecursive density n = fixProblem mutualRecursiveProblem (density (Node 2)) (Node n)

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
          [ testCase "stabilizes mutual recursive nodes" (fixMutualRecursive (const Sparse) 1 @?= 10)
          , testCase "stabilizes all nodes" (fixMutualRecursive (const Sparse) 2 @?= 10)
          ]
      , testGroup "Dense"
          [ testCase "stabilizes mutual recursive nodes" (fixMutualRecursive Dense 1 @?= 10)
          , testCase "stabilizes all nodes" (fixMutualRecursive Dense 2 @?= 10)
          ]
      ]
  ]

fibProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowProblem m
fibProblem = DFP transfer (const (eqChangeDetector p))
  where
    p :: Proxy m
    p = Proxy
    transfer :: Node -> TransferFunction m Natural
    transfer (Node 0) = return 0
    transfer (Node 1) = return 1
    transfer (Node n) = do
      a <- dependOn p (Node (n-1))
      b <- dependOn p (Node (n-2))
      return (a + b)

fib :: Int -> Natural
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

facProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowProblem m
facProblem = DFP transfer (const (eqChangeDetector p))
  where
    p :: Proxy m
    p = Proxy
    transfer :: Node -> TransferFunction m Natural
    transfer (Node 0) = return 1
    transfer (Node 1) = return 1
    transfer (Node n) = do
      a <- dependOn p (Node (n-1))
      return (fromIntegral n * a)

fac :: Int -> Natural
fac n = product [1..fromIntegral n]

mutualRecursiveProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowProblem m
mutualRecursiveProblem = DFP transfer (const (eqChangeDetector p))
  where
    p :: Proxy m
    p = Proxy
    transfer :: Node -> TransferFunction m Natural
    transfer (Node 0) = do
      b <- dependOn p (Node 1)
      return (b + 1)
    transfer (Node 1) = do
      a <- dependOn p (Node 0)
      return (min 10 a) -- So the overall fixpoint of this is 10
    transfer (Node 2) = dependOn p (Node 1)
    transfer _ = return 0
