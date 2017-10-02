module StrAnal where

import qualified Analyses.AdHocStrAnal          as AdHocStrAnal
import qualified Analyses.StrAnal               as StrAnal
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.MkCoreFromFile (compileCoreExpr)
import           Analyses.Syntax.MkCoreHelpers

import           Test.Tasty
import           Test.Tasty.HUnit

import           CoreSyn
import           CoreTidy                       (tidyExpr)
import           Id
import           VarEnv                         (emptyTidyEnv)

x, x1, x2, y, z, b, b1, b2, f, g :: Id
[x, x1, x2, y, z, b, b1, b2, f, g] = mkTestIds
  [ ("x", int)
  , ("x1", int)
  , ("x2", int)
  , ("y", int)
  , ("z", int)
  , ("b", bool)
  , ("b1", bool)
  , ("b2", bool)
  , ("f", bool2int2int)
  , ("g", bool2int2int)
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
anns1 = annotations (StrAnal.analyse example1)

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
StrLattice (ty2, anns2) = StrAnal.analyse example2

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
StrLattice (ty3, anns3) = StrAnal.analyse example3

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
StrLattice (ty4, _) = StrAnal.analyse example4

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
StrLattice (ty5, _) = StrAnal.analyse example5


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
StrLattice (_, anns6) = StrAnal.analyse example6


-- | @
-- let f b x =
--       if b
--         then f b z
--         else z
-- in f False 1
-- @
simpleRecursive1 :: CoreExpr
simpleRecursive1 = tidyExpr emptyTidyEnv $
  letrec
    f (lam b $ lam x $
        ite (var b)
          (var f $$ var b $$ var z)
          (var z))
    (var f $$ boolLit False $$ intLit 1)


-- | @
-- let f b1 x1 =
--       let g b2 x2 =
--             if b2
--               then g b2 z
--               else f b2 x2
--       in if b1
--            then g b1 x1
--            else z
-- in f False 1
-- @
nestedRecursive1 :: CoreExpr
nestedRecursive1 = tidyExpr emptyTidyEnv $
  letrec
    f (lam b1 $ lam x1 $
        letrec
          g (lam b2 $ lam x2 $
              ite (var b2)
                (var g $$ var b2 $$ var z)
                (var f $$ var b2 $$ var x2))
          (ite (var b)
            (var g $$ var b1 $$ var x1)
            (var z)))
    (var f $$ boolLit False $$ intLit 1)


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
  , coincidesWithAdHoc "simpleRecursive1" simpleRecursive1
  , coincidesWithAdHoc "nestedRecursive1" nestedRecursive1
  , coincidesWithAdHocOnFile "examples/exprs/const.hs"
  , coincidesWithAdHocOnFile "examples/exprs/findLT.hs"
  ] where
      coincidesWithAdHoc desc e =
        testGroup desc
          [ testCase "coincides with AdHocStrAnal" $
              StrAnal.analyse e @?= AdHocStrAnal.analyse e
          ]
      coincidesWithAdHocOnFile file =
        testGroup file
          [ testCase "coincides with AdHocStrAnal" $ do
              e <- compileCoreExpr file
              StrAnal.analyse e @?= AdHocStrAnal.analyse e
          ]

