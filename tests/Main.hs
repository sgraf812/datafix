import qualified Critical
import qualified StrAnal
import           System.Environment
import           Test.Tasty
import qualified Trivial

main :: IO ()
main = do
  -- Parallel tests get us in trouble because of
  -- locks on the package db in GHC 8.2.2
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain $ testGroup "All tests"
    [ testGroup "Unit test"
        [ testGroup "Trivial" Trivial.tests
        , testGroup "Critical cases" Critical.tests
        ]
    , testGroup "Analyses"
        [ testGroup "Strictness" StrAnal.tests
        ]
    ]
