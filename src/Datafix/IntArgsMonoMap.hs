{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Datafix.IntArgsMonoMap
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Composes 'IntMap' with a 'MonoMap'.

module Datafix.IntArgsMonoMap where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe         (maybeToList)
import           Datafix.MonoMap    (MonoMap, MonoMapKey)
import qualified Datafix.MonoMap    as MonoMap
import           GHC.Exts           (coerce)

newtype IntArgsMonoMap k v
  = Map (IntMap (MonoMap k v))

deriving instance Eq (MonoMap k v) => Eq (IntArgsMonoMap k v)
deriving instance Show (MonoMap k v) => Show (IntArgsMonoMap k v)

nothingIfEmpty :: MonoMapKey k => MonoMap k v -> Maybe (MonoMap k v)
nothingIfEmpty m
  | null m = Nothing
  | otherwise = Just m

empty :: IntArgsMonoMap k v
empty = Map IntMap.empty

singleton :: MonoMapKey k => Int -> k -> v -> IntArgsMonoMap k v
singleton i k v = Map (IntMap.singleton i (MonoMap.singleton k v))

insert :: MonoMapKey k => Int -> k -> v -> IntArgsMonoMap k v -> IntArgsMonoMap k v
insert i k v (Map m) =
  Map (IntMap.insertWith (const (MonoMap.insert k v)) i (MonoMap.singleton k v) m)

delete :: MonoMapKey k => Int -> k -> IntArgsMonoMap k v -> IntArgsMonoMap k v
delete i k (Map m) = Map (IntMap.update f i m)
  where
    f monoMap = nothingIfEmpty (MonoMap.delete k monoMap)

lookup :: MonoMapKey k => Int -> k -> IntArgsMonoMap k v -> Maybe v
lookup i k (Map m) = IntMap.lookup i m >>= MonoMap.lookup k

difference :: MonoMapKey k => IntArgsMonoMap k a -> IntArgsMonoMap k b -> IntArgsMonoMap k a
difference (Map ma) (Map mb) = Map (IntMap.differenceWith f ma mb)
  where
    f a b = nothingIfEmpty (MonoMap.difference a b)

-- | Highest priority node and lowest element of the domain `k` first.
highestPriorityNodes :: MonoMapKey k => IntArgsMonoMap k v -> [(Int, k)]
highestPriorityNodes (Map m) = maybeToList (IntMap.maxViewWithKey m) >>= viewIntoMonoMap
  where
    viewIntoMonoMap ((i, monoMap), _) = pairUp i <$> MonoMap.lookupMin monoMap
    pairUp i (k, _) = (i, k)

keys :: MonoMapKey k => IntArgsMonoMap k v -> [(Int, k)]
keys (Map m) = IntMap.foldrWithKey f [] m
  where
    f i monoMap ks = map ((,) i) (MonoMap.keys monoMap) ++ ks

insertLookupWithKey
  :: MonoMapKey k
  => (Int -> k -> v -> v -> v)
  -> Int
  -> k
  -> v
  -> IntArgsMonoMap k v
  -> (Maybe v, IntArgsMonoMap k v)
insertLookupWithKey f i k v (Map m) = coerce (IntMap.alterF alterMonoMap i m)
  where
    alterMonoMap Nothing        = (Nothing, Just (MonoMap.singleton k v))
    alterMonoMap (Just monoMap) = Just <$> MonoMap.insertLookupWithKey (f i) k v monoMap

insertWith
  :: MonoMapKey k
  => (v -> v -> v)
  -> Int
  -> k
  -> v
  -> IntArgsMonoMap k v
  -> IntArgsMonoMap k v
insertWith f i k v (Map m) = Map $
  IntMap.insertWith (const $ MonoMap.insertWith f k v) i (MonoMap.singleton k v) m

updateLookupWithKey
  :: MonoMapKey k
  => (Int -> k -> v -> Maybe v)
  -> Int
  -> k
  -> IntArgsMonoMap k v
  -> (Maybe v, IntArgsMonoMap k v)
updateLookupWithKey f i k (Map m) = coerce (IntMap.alterF alterMonoMap i m)
  where
    alterMonoMap Nothing        = (Nothing, Nothing)
    alterMonoMap (Just monoMap) = nothingIfEmpty <$> MonoMap.updateLookupWithKey (f i) k monoMap

adjust :: MonoMapKey k => (v -> v) -> Int -> k -> IntArgsMonoMap k v -> IntArgsMonoMap k v
adjust f i k (Map m) = Map (IntMap.adjust (MonoMap.adjust f k) i m)

lookupLT :: MonoMapKey k => Int -> k -> IntArgsMonoMap k v -> [(k, v)]
lookupLT i k (Map m) = maybe [] (MonoMap.lookupLT k) (IntMap.lookup i m)
