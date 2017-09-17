{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Datafix.MonoMap where

import           Algebra.PartialOrd
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe         (maybeToList)
import           Data.POMap.Strict  (POMap)
import qualified Data.POMap.Strict  as POMap

type MonoSet k = MonoMap k ()

class Foldable (MonoMap k) => MonoMapKey k where
  type MonoMap k = (r :: * -> *) | r -> k
  type MonoMap k = POMap k
  empty :: MonoMap k v
  default empty :: (MonoMap k v ~ POMap k v) => MonoMap k v
  empty = POMap.empty
  singleton :: k -> v -> MonoMap k v
  default singleton :: (MonoMap k v ~ POMap k v) => k -> v -> MonoMap k v
  singleton = POMap.singleton
  insert :: k -> v -> MonoMap k v -> MonoMap k v
  default insert :: (MonoMap k v ~ POMap k v, PartialOrd k) => k -> v -> MonoMap k v -> MonoMap k v
  insert = POMap.insert
  delete :: k -> MonoMap k v -> MonoMap k v
  default delete :: (MonoMap k v ~ POMap k v, PartialOrd k) => k -> MonoMap k v -> MonoMap k v
  delete = POMap.delete
  lookup :: k -> MonoMap k v -> Maybe v
  default lookup :: (MonoMap k v ~ POMap k v, PartialOrd k) => k -> MonoMap k v -> Maybe v
  lookup = POMap.lookup
  lookupLE :: k -> MonoMap k v -> [(k, v)]
  default lookupLE :: (MonoMap k v ~ POMap k v, PartialOrd k) => k -> MonoMap k v -> [(k, v)]
  lookupLE = POMap.lookupLE
  lookupMin :: MonoMap k v -> [(k, v)]
  default lookupMin :: (MonoMap k v ~ POMap k v, PartialOrd k) => MonoMap k v -> [(k, v)]
  lookupMin = POMap.lookupMin
  difference :: MonoMap k a -> MonoMap k b -> MonoMap k a
  default difference :: (MonoMap k a ~ POMap k a, MonoMap k b ~ POMap k b, PartialOrd k) => MonoMap k a -> MonoMap k b -> MonoMap k a
  difference = POMap.difference
  keys :: MonoMap k a -> [k]
  default keys :: MonoMap k v ~ POMap k v => POMap k v -> [k]
  keys = POMap.keys
  insertLookupWithKey :: (k -> v -> v -> v) -> k -> v -> MonoMap k v -> (Maybe v, MonoMap k v)
  default insertLookupWithKey :: (MonoMap k v ~ POMap k v, PartialOrd k) => (k -> v -> v -> v) -> k -> v -> POMap k v -> (Maybe v, POMap k v)
  insertLookupWithKey = POMap.insertLookupWithKey
  alter :: (Maybe v -> Maybe v) -> k -> MonoMap k v -> MonoMap k v
  default alter :: (MonoMap k v ~ POMap k v, PartialOrd k) => (Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v
  alter = POMap.alter

instance MonoMapKey () where
  type MonoMap () = Maybe
  empty = Nothing
  singleton _ = Just
  insert _ v _ = Just v
  delete _ _ = Nothing
  lookup _ m = m
  lookupLE _ = fmap ((,) ()) . maybeToList
  lookupMin = lookupLE ()
  difference _ (Just _) = Nothing
  difference a _        = a
  keys _ = [()]
  insertLookupWithKey _ _ v Nothing    = (Nothing, Just v)
  insertLookupWithKey f _ v (Just old) = (Just old, Just (f () v old))
  alter f _ = f

instance MonoMapKey Int where
  type MonoMap Int = IntMap
  empty = IntMap.empty
  singleton = IntMap.singleton
  insert = IntMap.insert
  delete = IntMap.delete
  lookup = IntMap.lookup
  lookupLE k = maybeToList . IntMap.lookupLE k
  lookupMin = maybeToList . fmap fst . IntMap.minViewWithKey
  difference = IntMap.difference
  keys = IntMap.keys
  insertLookupWithKey = IntMap.insertLookupWithKey
  alter = IntMap.alter

instance PartialOrd () where
  leq _ _ = True
