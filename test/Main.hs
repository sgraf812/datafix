import qualified Analyses.Tests.StrAnal as StrAnal
import qualified Trivial
import qualified Critical
import           Test.Tasty

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