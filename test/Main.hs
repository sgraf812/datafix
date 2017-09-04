import qualified Data.Map.Strict  as Map
import           Data.Maybe       (fromJust, fromMaybe)
import qualified Data.Set         as Set
import           Datafix
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "All tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests =
  [
  ]

huTests :: [TestTree]
huTests =
  [ testGroup "Memoization"
      [ testCase "fibonacci 10" (evaluate fibProblem 10 @?= fib 10)
      , testCase "factorial 100" (evaluate facProblem 100 @?= fac 100)
      ]
  , testGroup "mutual recursion"
      [ testCase "stabilizes mutual recursive nodes" (evaluate mutualRecursiveProblem 1 @?= 10)
      , testCase "stabilizes all nodes" (evaluate mutualRecursiveProblem 2 @?= 10)
      ]
  ]

evaluate :: Ord a => DataFlowProblem a b -> a -> b
evaluate fw a = (fromJust . Map.lookup a . fixProblem fw . Set.singleton) a

fibProblem :: DataFlowProblem Int Integer
fibProblem = DFP transfer (const eqChangeDetector)
  where
    transfer :: Int -> TransferFunction Int Integer Integer
    transfer 0 = return 0
    transfer 1 = return 1
    transfer n = do
      Just a <- dependOn (n-1)
      Just b <- dependOn (n-2)
      return (a + b)

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

facProblem :: DataFlowProblem Int Integer
facProblem = DFP transfer (const eqChangeDetector)
  where
    transfer :: Int -> TransferFunction Int Integer Integer
    transfer 0 = return 1
    transfer 1 = return 1
    transfer n = do
      Just a <- dependOn (n-1)
      return (fromIntegral n * a)

fac :: Int -> Integer
fac n = product [1..fromIntegral n]

mutualRecursiveProblem :: DataFlowProblem Int Int
mutualRecursiveProblem = DFP transfer (const eqChangeDetector)
  where
    transfer :: Int -> TransferFunction Int Int Int
    transfer 0 = do
      b <- fromMaybe 0 <$> dependOn 1
      return (b + 1)
    transfer 1 = do
      a <- fromMaybe 0 <$> dependOn 0
      return (min 10 a) -- So the overall fixpoint of this is 10
    transfer 2 = fromMaybe 0 <$> dependOn 1
    transfer _ = return 0
