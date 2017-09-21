module Analyses.Tests.StrAnal where

import           Analyses.StrAnal.Analysis
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.MkCoreHelpers

import           Test.Tasty
import           Test.Tasty.HUnit

import           CoreSyn
import           Id

x, y, z, b, f, g :: Id
[x, y, z, b, f, g] = mkTestIds
  [ ("x", int)
  , ("y", int)
  , ("z", int)
  , ("b", bool)
  , ("f", bool2int2int)
  , ("g", int2int)
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

anns1 :: Annotations
anns1 = annotations (analyse example1)

-- | @
-- let f b =
--       if b
--         then \x -> z
--         else \y -> z
-- in f False 1
-- @
example2 :: CoreExpr
example2 =
  letrec
    f (lam b $
        ite (var b)
          (lam x (var z))
          (lam y (var z)))
    (var f $$ boolLit False $$ intLit 1)

ty2 :: StrType
anns2 :: Annotations
StrLattice (ty2, anns2) = analyse example2

-- | @
-- let f b =
--       if b
--         then f b
--         else \y -> z
-- in f False 1
-- @
example3 :: CoreExpr
example3 =
  letrec
    f (lam b $
        ite (var b)
          (var f $$ var b)
          (lam y (var z)))
    (var f $$ boolLit False $$ intLit 1)

ty3 :: StrType
anns3 :: Annotations
StrLattice (ty3, anns3) = analyse example3

-- | @
-- let f b =
--       if b
--         then \x -> f b z
--         else \y -> z
-- in f False 1
-- @
example4 :: CoreExpr
example4 =
  letrec
    f (lam b $
        ite (var b)
          (lam x (var f $$ var b $$ var z))
          (lam y (var z)))
    (var f $$ boolLit False $$ intLit 1)

ty4 :: StrType
StrLattice (ty4, _) = analyse example4

-- | @
-- let f b =
--       if b
--         then \x -> f b z
--         else \y -> 0
-- in f False 1
-- @
example5 :: CoreExpr
example5 =
  letrec
    f (lam b $
        ite (var b)
          (lam x (var f $$ var b $$ var z))
          (lam y (intLit 0)))
    (var f $$ boolLit False $$ intLit 1)

ty5 :: StrType
StrLattice (ty5, _) = analyse example5


-- | @
-- let f b =
--       if b
--         then f b
--         else \y -> y
-- in f False 1
-- @
example6 :: CoreExpr
example6 =
  letrec
    f (lam b $
        ite (var b)
          (var f $$ var b)
          (lam y (var y)))
    (var f $$ boolLit False $$ intLit 1)

anns6 :: Annotations
StrLattice (_, anns6) = analyse example6

tests :: [TestTree]
tests =
  [ testGroup "example1"
      [ testCase "f is called strictly with two args" $
          lookupAnnotation f anns1 @?= Just (Strict 2)
      , testCase "b is evaluated strictly" $
          lookupAnnotation b anns1 @?= Just (Strict 0)
      , testCase "y is evaluated strictly" $
          lookupAnnotation y anns1 @?= Just (Strict 0)
      , testCase "z is evaluated strictly" $
          lookupAnnotation z anns1 @?= Just (Strict 0)
      ]
  , testGroup "example2"
      [ testCase "f is called strictly with two args" $
          lookupAnnotation f anns2 @?= Just (Strict 2)
      , testCase "x is evaluated lazily" $
          lookupAnnotation x anns2 @?= Just Lazy
      , testCase "y is evaluated lazily" $
          lookupAnnotation y anns2 @?= Just Lazy
      , testCase "fv z is evaluated strictly" $
          fst (peelFV z ty2) @?= Strict 0
      ]
  , testGroup "example3"
      [ testCase "f is called strictly with two args" $
          lookupAnnotation f anns3 @?= Just (Strict 2)
      , testCase "b is evaluated strictly" $
          lookupAnnotation b anns3 @?= Just (Strict 0)
      , testCase "y is evaluated lazily" $
          lookupAnnotation y anns3 @?= Just Lazy
      , testCase "fv z is evaluated strictly" $
          fst (peelFV z ty3) @?= Strict 0
      ]
  , testGroup "example4"
      [ testCase "fv z is evaluated strictly" $
          fst (peelFV z ty4) @?= Strict 0
      ]
  , testGroup "example5"
      [ testCase "fv z is evaluated lazily" $
          fst (peelFV z ty5) @?= Lazy
      ]
  , testGroup "example6"
      [ testCase "y is evaluated strictly" $
          lookupAnnotation y anns6 @?= Just (Strict 0)
      ]
  ]