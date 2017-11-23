{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      :  Datafix.MonoMap
-- Copyright   :  (c) Sebastian Graf 2017
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Helpers for allocating 'Node's in an ergonomic manner, e.g.
-- taking care to get 'mfix' right under the hood for allocation
-- in recursive bindings groups through the key primitive 'allocateNode'.

module Datafix.MonoMap where

import           Algebra.PartialOrd
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe         (maybeToList)
import           Data.POMap.Strict  (POMap)
import qualified Data.POMap.Strict  as POMap

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
  lookupLT :: k -> MonoMap k v -> [(k, v)]
  default lookupLT :: (MonoMap k v ~ POMap k v, PartialOrd k) => k -> MonoMap k v -> [(k, v)]
  lookupLT = POMap.lookupLT
  lookupMin :: MonoMap k v -> [(k, v)]
  default lookupMin :: (MonoMap k v ~ POMap k v, PartialOrd k) => MonoMap k v -> [(k, v)]
  lookupMin = POMap.lookupMin
  difference :: MonoMap k a -> MonoMap k b -> MonoMap k a
  default difference :: (MonoMap k a ~ POMap k a, MonoMap k b ~ POMap k b, PartialOrd k) => MonoMap k a -> MonoMap k b -> MonoMap k a
  difference = POMap.difference
  keys :: MonoMap k a -> [k]
  default keys :: MonoMap k v ~ POMap k v => MonoMap k v -> [k]
  keys = POMap.keys
  insertWith :: (v -> v -> v) -> k -> v -> MonoMap k v -> MonoMap k v
  default insertWith :: (MonoMap k v ~ POMap k v, PartialOrd k) => (v -> v -> v) -> k -> v -> MonoMap k v -> MonoMap k v
  insertWith = POMap.insertWith
  insertLookupWithKey :: (k -> v -> v -> v) -> k -> v -> MonoMap k v -> (Maybe v, MonoMap k v)
  default insertLookupWithKey :: (MonoMap k v ~ POMap k v, PartialOrd k) => (k -> v -> v -> v) -> k -> v -> MonoMap k v -> (Maybe v, MonoMap k v)
  insertLookupWithKey = POMap.insertLookupWithKey
  updateLookupWithKey :: (k -> v -> Maybe v) -> k -> MonoMap k v -> (Maybe v, MonoMap k v)
  default updateLookupWithKey :: (MonoMap k v ~ POMap k v, PartialOrd k) => (k -> v -> Maybe v) -> k -> MonoMap k v -> (Maybe v, MonoMap k v)
  updateLookupWithKey = POMap.updateLookupWithKey
  alter :: (Maybe v -> Maybe v) -> k -> MonoMap k v -> MonoMap k v
  default alter :: (MonoMap k v ~ POMap k v, PartialOrd k) => (Maybe v -> Maybe v) -> k -> MonoMap k v -> MonoMap k v
  alter = POMap.alter
  adjust :: (v -> v) -> k -> MonoMap k v -> MonoMap k v
  default adjust :: (MonoMap k v ~ POMap k v, PartialOrd k) => (v -> v) -> k -> MonoMap k v -> MonoMap k v
  adjust = POMap.adjust

instance MonoMapKey () where
  type MonoMap () = Maybe
  empty = Nothing
  singleton _ = Just
  insert _ v _ = Just v
  delete _ _ = Nothing
  lookup _ m = m
  lookupLT _ = fmap ((,) ()) . maybeToList
  lookupMin = lookupLT ()
  difference _ (Just _) = Nothing
  difference a _        = a
  keys _ = [()]
  insertWith _ _ v Nothing    = Just v
  insertWith f _ v (Just old) = Just (f v old)
  insertLookupWithKey _ _ v Nothing    = (Nothing, Just v)
  insertLookupWithKey f _ v (Just old) = (Just old, Just (f () v old))
  updateLookupWithKey _ _ Nothing    = (Nothing, Nothing)
  updateLookupWithKey f _ (Just old) = (Just old, f () old)
  alter f _ = f
  adjust f _ = fmap f

instance MonoMapKey Int where
  type MonoMap Int = IntMap
  empty = IntMap.empty
  singleton = IntMap.singleton
  insert = IntMap.insert
  delete = IntMap.delete
  lookup = IntMap.lookup
  lookupLT k = maybeToList . IntMap.lookupLT k
  lookupMin = maybeToList . fmap fst . IntMap.minViewWithKey
  difference = IntMap.difference
  keys = IntMap.keys
  insertWith = IntMap.insertWith
  insertLookupWithKey = IntMap.insertLookupWithKey
  updateLookupWithKey = IntMap.updateLookupWithKey
  alter = IntMap.alter
  adjust = IntMap.adjust
