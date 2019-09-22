{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module SetRecurrences.FirstFollow
  ( V (..)
  , WithEOF (..)
  , Grammar (..)
  , mkGrammar
  , augmentGrammar
  , first
  , follow
  , dyck
  , llsll
  , emptyL
  , leftrec
  , main
  ) where

import           Prelude hiding (words)

import           Datafix

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor (second)
import           Data.Functor.Identity
import           Data.List (unfoldr)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           System.Environment
import           Text.Printf

data V t nt
  = T t
  | NT nt
  deriving (Eq, Ord, Show)

eitherV :: (t -> r) -> (nt -> r) -> V t nt -> r
eitherV l _ (T t)   = l t
eitherV _ r (NT nt) = r nt

data WithEOF a
  = EOF
  | NoEOF a
  deriving (Eq, Ord, Show)

instance {-# OVERLAPPING #-} Show [WithEOF Char] where
  show = show . map f
    where
      f EOF       = '#'
      f (NoEOF c) = c

data Grammar t nt
  = G
  { terminals :: Set t
  , nonterminals :: Set nt
  , productions :: Map nt (Set [V t nt])
  , start :: nt
  }

mkGrammar :: (Ord v) => [v] -> [v] -> [(v, [v])] -> v -> Grammar v v
mkGrammar ts nts ps s
  = G
  { terminals    = Set.fromList ts
  , nonterminals = Set.fromList nts
  , productions  = Map.fromListWith Set.union (map (second (Set.singleton . map mkV)) ps)
  , start        = s
  }
  where
    mkV v
      | elem v ts  = T v
      | elem v nts = NT v
      | otherwise  = error "not part of vocabulary"

augmentGrammar :: (Ord t, Ord nt) => nt -> Grammar t nt -> Grammar t nt
augmentGrammar s' gr
  = G
  { terminals    = terminals gr
  , nonterminals = Set.insert s' (nonterminals gr)
  , productions  = Map.insert s' (Set.singleton [NT (start gr)]) $ productions gr
  , start        = s'
  }

epsilon :: [t]
epsilon = []

kconcat :: Int -> [a] -> [a] -> [a]
kconcat k pref suf = take k (pref ++ suf)

pointwise2 :: Ord c => (a -> b -> c) -> Set a -> Set b -> Set c
pointwise2 f as bs = Set.fromList (liftA2 f (Set.toList as) (Set.toList bs))

-- |
-- NB: A Monad here is strictly more performant than Applicative here, because
-- we can exit early. On the other this means we have this really adhoc fold over
subst :: (Monad m, Ord t) => Int -> (nt -> m (Set [t])) -> [V t nt] -> m (Set [t])
subst k lkup = go (Set.singleton epsilon)
  where
    go prefs [] = pure prefs
    go prefs (v:vs) = do
      suffs <- eitherV (pure . Set.singleton . (:[])) lkup v
      let words = pointwise2 (kconcat k) prefs suffs
      if any ((< k) . length) words
        then go words vs
        else pure words

first :: forall t nt . (Ord t, Ord nt, Datafixable (Set [t])) => Int -> Grammar t nt -> [V t nt] -> Set [t]
first k gr = runIdentity . subst k (Identity . (firstSolutions k gr Map.!))

firstSolutions :: forall t nt . (Ord t, Ord nt, Datafixable (Set [t])) => Int -> Grammar t nt -> Map nt (Set [t])
firstSolutions k gr = evalDenotation @(Set [t]) @(Map nt (Set [t])) plan NeverAbort
  where
    plan :: forall m . (MonadDatafix m, Domain (DepM m) ~ Set [t]) => m (DepM m (Map nt (Set [t])))
    plan = sequence <$> build Map.empty (Set.toList (nonterminals gr))
    build :: forall m . (MonadDatafix m, Domain (DepM m) ~ Set [t]) => Map nt (DepM m (Set [t])) -> [nt] -> m (Map nt (DepM m (Set [t])))
    build env []       = pure env
    build env (nt:nts) = datafixEq $ \self -> do
      env' <- build (Map.insert nt self env) nts
      let rhss = Set.toList (fromMaybe Set.empty (Map.lookup nt (productions gr)))
      let iter = Set.unions <$> mapM (subst k (env' Map.!)) rhss
      pure (env', iter)

-- | Assumes the given grammar is augmented
follow :: forall t nt . (Ord t, Ord nt, Datafixable (Set [WithEOF t])) => Int -> Grammar t nt -> nt -> Set [WithEOF t]
follow k gr = (evalDenotation @(Set [WithEOF t]) plan NeverAbort Map.!)
  where
    prods :: [(nt, [V t nt])]
    prods =
      [ (nt, rhs)
      | (nt, rhss) <- Map.toList (productions gr)
      , rhs <- Set.toList rhss
      ]
    firsts = firstSolutions k gr
    initialEnv :: forall m . Monad m => Map nt (m (Set [WithEOF t]))
    initialEnv = Map.singleton (start gr) (pure (Set.singleton (take k (repeat EOF))))
    plan :: forall m . (MonadDatafix m, Domain (DepM m) ~ Set [WithEOF t]) => m (DepM m (Map nt (Set [WithEOF t])))
    plan = sequence <$> build initialEnv (Set.toList (Set.delete (start gr) (nonterminals gr)))
    build :: forall m . (MonadDatafix m, Domain (DepM m) ~ Set [WithEOF t]) => Map nt (DepM m (Set [WithEOF t])) -> [nt] -> m (Map nt (DepM m (Set [WithEOF t])))
    build env []       = pure env
    build env (nt:nts) = datafixEq $ \self -> do
      env' <- build (Map.insert nt self env) nts
      let occs = flip concatMap prods $ \(parent, rhs) -> flip unfoldr rhs $ \rest ->
            case dropWhile (/= NT nt) rest of
              [] -> Nothing
              (_:follows) -> Just ((parent, follows), follows)

      let iter = fmap Set.unions $ forM occs $ \(parent, follows) -> do
            let prefs = Set.map (map NoEOF) $ runIdentity $ subst k (Identity . (firsts Map.!)) follows
            let (short, long) = Set.partition ((< k) . length) prefs
            if null short
              then pure prefs
              else Set.union long . pointwise2 (kconcat k) prefs <$> (env' Map.! parent)

      pure (env', iter)

dyck :: Grammar Char Char
dyck = augmentGrammar 'O' $ mkGrammar
  "()"
  "S"
  [ ('S', "")
  , ('S', "(S)S")
  ]
  'S'

llsll :: Grammar Char Char
llsll = augmentGrammar 'O' $ mkGrammar
  "ab"
  "ZA"
  [ ('Z', "aAab")
  , ('Z', "bAb")
  , ('A', "a")
  , ('A', "")
  ]
  'Z'

emptyL :: Grammar Char Char
emptyL = augmentGrammar 'O' $ mkGrammar
  ""
  "ABC"
  [ ('A', "BC")
  , ('B', "CA")
  , ('C', "A")
  ]
  'A'

leftrec :: Grammar Char Char
leftrec = augmentGrammar 'O' $ mkGrammar
  "abc"
  "SA"
  [ ('S', "Aa")
  , ('A', "Sb")
  , ('A', "c")
  ]
  'S'

main :: IO ()
main = do
  (k:_) <- map read <$> getArgs
  let uncurry3 f (x,y,z) = f x y z
  let analyse name gr (s :: Char) = do
        printf "%s:\n" name
        printf "  first_%d(%s): %s\n" k (show s) (show $ first k gr [NT s])
        printf "  follow_%d(%s): %s\n" k (show s) (show $ follow k gr s)
        putStrLn ""
  mapM_ (uncurry3 analyse) $
    [ ("Dyck", dyck, 'S')
    , ("LL(1), not SLL(k)", llsll, 'A')
    , ("empty", emptyL, 'A')
    , ("left recursive", leftrec, 'S')
    ]
