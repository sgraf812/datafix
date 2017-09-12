import           Algebra.Lattice
import qualified Analyses.Tests.StrAnal as StrAnal
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromJust, fromMaybe)
import qualified Data.Set               as Set
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
      [ testCase "fibonacci 10" (fixProblem fibProblem 10 @?= fib 10)
      , testCase "factorial 100" (fixProblem facProblem 100 @?= fac 100)
      ]
  , testGroup "mutual recursion"
      [ testCase "stabilizes mutual recursive nodes" (fixProblem mutualRecursiveProblem 1 @?= 10)
      , testCase "stabilizes all nodes" (fixProblem mutualRecursiveProblem 2 @?= 10)
      ]
  ]

fibProblem :: DataFlowProblem Int Natural
fibProblem = DFP transfer (const eqChangeDetector)
  where
    transfer :: Int -> TransferFunction Int Natural Natural
    transfer 0 = return 0
    transfer 1 = return 1
    transfer n = do
      a <- dependOn (n-1)
      b <- dependOn (n-2)
      return (a + b)

fib :: Int -> Natural
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

facProblem :: DataFlowProblem Int Natural
facProblem = DFP transfer (const eqChangeDetector)
  where
    transfer :: Int -> TransferFunction Int Natural Natural
    transfer 0 = return 1
    transfer 1 = return 1
    transfer n = do
      a <- dependOn (n-1)
      return (fromIntegral n * a)

fac :: Int -> Natural
fac n = product [1..fromIntegral n]

mutualRecursiveProblem :: DataFlowProblem Int Natural
mutualRecursiveProblem = DFP transfer (const eqChangeDetector)
  where
    transfer :: Int -> TransferFunction Int Natural Natural
    transfer 0 = do
      b <- dependOn 1
      return (b + 1)
    transfer 1 = do
      a <- dependOn 0
      return (min 10 a) -- So the overall fixpoint of this is 10
    transfer 2 = dependOn 1
    transfer _ = return 0
