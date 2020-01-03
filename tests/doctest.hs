import Build_doctests
import Test.DocTest
import System.Environment (unsetEnv)

main :: IO ()
main = do
  unsetEnv "GHC_ENVIRONMENT"
  let args = flags ++ pkgs ++ module_sources
  doctest args
