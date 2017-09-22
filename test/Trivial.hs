{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Trivial (tests) where

import           Algebra.Lattice
import           Data.Proxy
import           Datafix
import           Datafix.Worklist (fixProblem)
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.HUnit

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

tests :: [TestTree]
tests =
  [ testGroup "Memoization"
      [ testCase "fibonacci 10" (fixProblem id fibProblem (Node 10) @?= fib 10)
      , testCase "factorial 100" (fixProblem id facProblem (Node 100) @?= fac 100)
      ]
  , testGroup "mutual recursion"
      [ testCase "stabilizes mutual recursive nodes" (fixProblem id mutualRecursiveProblem (Node 1) @?= 10)
      , testCase "stabilizes all nodes" (fixProblem id mutualRecursiveProblem (Node 2) @?= 10)
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
