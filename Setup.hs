import           Distribution.Extra.Doctest
import           Distribution.Simple
import           Distribution.Simple.Toolkit

main :: IO ()
main = defaultMainWithHooks simpleUserHooksWithBuildInfo
  { buildHook = \pkg lbi hooks flags -> do
      generateBuildModule "doctests" flags pkg lbi
      buildHook simpleUserHooks pkg lbi hooks flags
  }
