{-# LANGUAGE FlexibleContexts #-}

module FirstFollow where

import           Prelude hiding (seq, exp)

import           SetRecurrences.FirstFollow
import qualified Data.Set as Set
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "Dyck"
      [ testFirst 3 dyck "S(S)" ["(((", "(()", "()", "()("]
      , testFollow 3 dyck 'S' ["###",")##",")((",")()","))#","))(",")))"]
      ]
  , testGroup "LL(1), not SLL(k)"
      [ testFirst 3 llsll "A" ["","a"]
      , testFollow 3 llsll 'A' ["ab#","b##"]
      ]
  , testGroup "empty"
      [ testFirst 3 emptyL "A" []
      , testFollow 3 emptyL 'A' ["###"]
      ]
  , testGroup "Left recursion"
      [ testFirst 3 leftrec "A" ["c","cab"]
      , testFollow 3 leftrec 'A' ["a##","aba"]
      ]
  ] where
      testFirst :: Int -> Grammar Char Char -> String -> [String] -> TestTree
      testFirst k gr seq exp =
        testCase ("First_" ++ show k ++ "('" ++ seq ++ "')") $
          first k gr (map (mkV gr) seq) @?= Set.fromList exp
      testFollow :: Int -> Grammar Char Char -> Char -> [String] -> TestTree
      testFollow k gr nt exp =
        testCase ("Follow_" ++ show k ++ "(" ++ show nt ++ ")") $
          follow k gr nt @?= Set.fromList (map (map mkWithEOF) exp)
      mkV gr v
        | elem v (terminals gr)    = T v
        | elem v (nonterminals gr) = NT v
        | otherwise                = error "not part of vocabulary"
      mkWithEOF '#' = EOF
      mkWithEOF c   = NoEOF c
