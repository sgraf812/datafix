import qualified Critical
import qualified StrAnal
import           Test.Tasty
import qualified Trivial

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ testGroup "Unit test"
      [ testGroup "Trivial" Trivial.tests
      , testGroup "Critical cases" Critical.tests
      ]
  , testGroup "Analyses"
      [ testGroup "Strictness" StrAnal.tests
      ]
  ]
