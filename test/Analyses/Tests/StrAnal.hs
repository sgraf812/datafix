module Analyses.Tests.StrAnal where

import           Analyses.StrAnal.Analysis
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.MkCoreHelpers

import           Test.Tasty
import           Test.Tasty.HUnit

import           CoreSyn
import           Id

x, y, z, b, f :: Id
[x, y, z, b, f] = mkTestIds
  [ ("x", int)
  , ("y", int)
  , ("z", int)
  , ("b", bool)
  , ("f", bool2int2int)
  ]

-- | @
-- let f b =
--       if b
--         then \y -> y
--         else \z -> z
-- in f False 1
-- @
example1 :: CoreExpr
example1 =
  letrec
    f (lam b $
        ite (var b)
          (lam y (var y))
          (lam z (var z)))
    (var f $$ boolLit False $$ intLit 1)

result1 :: Annotations
result1 = annotations (analyse example1)

tests :: [TestTree]
tests =
  [ testGroup "example1"
      [ testCase "f is called strictly with two args" $
          lookupAnnotation f result1 @=? Just (Strict 2)
      , testCase "b is evaluated strictly" $
          lookupAnnotation b result1 @=? Just (Strict 0)
      , testCase "y is evaluated strictly" $
          lookupAnnotation y result1 @=? Just (Strict 0)
      , testCase "z is evaluated strictly" $
          lookupAnnotation z result1 @=? Just (Strict 0)
      ]
  ]
