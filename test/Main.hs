{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Algebra.Lattice
import qualified Analyses.Tests.StrAnal as StrAnal
import           Data.Proxy
import           Datafix
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.HUnit

instance JoinSemiLattice Natural where
  (\/) = max

instance BoundedJoinSemiLattice Natural where
  bottom = 0

main :: IO ()
main = defaultMain $ testGroup "All tests" tests

tests :: [TestTree]
tests =
  [ testGroup "Unit tests" huTests
  , testGroup "Analyses"
      [ testGroup "Strictness" StrAnal.tests
      ]
  ]

huTests :: [TestTree]
huTests =
  [ testGroup "Memoization"
      [ testCase "fibonacci 10" (fixProblem fibProblem (GraphNode 10) @?= fib 10)
      , testCase "factorial 100" (fixProblem facProblem (GraphNode 100) @?= fac 100)
      ]
  , testGroup "mutual recursion"
      [ testCase "stabilizes mutual recursive nodes" (fixProblem mutualRecursiveProblem (GraphNode 1) @?= 10)
      , testCase "stabilizes all nodes" (fixProblem mutualRecursiveProblem (GraphNode 2) @?= 10)
      ]
  ]

p :: Proxy Natural
p = Proxy

fibProblem :: DataFlowProblem Natural
fibProblem = DFP transfer (const (eqChangeDetector (Proxy :: Proxy Natural)))
  where
    transfer :: GraphNode -> TransferFunction (DependencyM Natural) Natural
    transfer (GraphNode 0) = return 0
    transfer (GraphNode 1) = return 1
    transfer (GraphNode n) = do
      a <- dependOn p (GraphNode (n-1))
      b <- dependOn p (GraphNode (n-2))
      return (a + b)

fib :: Int -> Natural
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

facProblem :: DataFlowProblem Natural
facProblem = DFP transfer (const (eqChangeDetector (Proxy :: Proxy Natural)))
  where
    transfer :: GraphNode -> TransferFunction (DependencyM Natural) Natural
    transfer (GraphNode 0) = return 1
    transfer (GraphNode 1) = return 1
    transfer (GraphNode n) = do
      a <- dependOn p (GraphNode (n-1))
      return (fromIntegral n * a)

fac :: Int -> Natural
fac n = product [1..fromIntegral n]

mutualRecursiveProblem :: DataFlowProblem Natural
mutualRecursiveProblem = DFP transfer (const (eqChangeDetector (Proxy :: Proxy Natural)))
  where
    transfer :: GraphNode -> TransferFunction (DependencyM Natural) Natural
    transfer (GraphNode 0) = do
      b <- dependOn p (GraphNode 1)
      return (b + 1)
    transfer (GraphNode 1) = do
      a <- dependOn p (GraphNode 0)
      return (min 10 a) -- So the overall fixpoint of this is 10
    transfer (GraphNode 2) = dependOn p (GraphNode 1)
    transfer _ = return 0
