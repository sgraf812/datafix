import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, fromMaybe)
import Worklist
import Debug.Trace

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
      [ testCase "fibonacci 10" (evaluate fibFramework 10 @?= fib 10)
      , testCase "factorial 100" (evaluate facFramework 100 @?= fac 100)
      ]
  , testCase "mutual recursion" (evaluate mutualRecursiveFramework 1 @?= 10)
  ]

evaluate :: (Show a, Show b, Ord a, Eq b) => DataFlowFramework a b -> a -> b
evaluate fw a = (fromJust . Map.lookup a . runFramework fw . Set.singleton) a

fibFramework :: DataFlowFramework Int Integer
fibFramework = frameworkWithEqChangeDetector transfer
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

facFramework :: DataFlowFramework Int Integer
facFramework = frameworkWithEqChangeDetector transfer
  where
    transfer :: Int -> TransferFunction Int Integer Integer
    transfer 0 = return 1
    transfer 1 = return 1
    transfer n = do
      Just a <- dependOn (n-1)
      return (fromIntegral n * a)

fac :: Int -> Integer
fac n = product [1..fromIntegral n]

mutualRecursiveFramework :: DataFlowFramework Int Int
mutualRecursiveFramework = frameworkWithEqChangeDetector transfer
  where
    transfer :: Int -> TransferFunction Int Int Int
    transfer 0 = do
      b <- fromMaybe 0 <$> dependOn 1
      return (b + 1)
    transfer 1 = do
      a <- fromMaybe 0 <$> dependOn 0
      return (min 10 a) -- So the overall fixpoint of this is 10
    transfer _ = return 0
