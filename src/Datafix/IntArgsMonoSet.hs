{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Datafix.IntArgsMonoSet where

import           Data.Maybe             (isJust)
import           Datafix.IntArgsMonoMap (IntArgsMonoMap)
import qualified Datafix.IntArgsMonoMap as Map
import           Datafix.MonoMap        (MonoMapKey)
import           GHC.Exts               (coerce)

newtype IntArgsMonoSet k
  = Set (IntArgsMonoMap k ())

deriving instance Eq (IntArgsMonoMap k ()) => Eq (IntArgsMonoSet k)
deriving instance Show (IntArgsMonoMap k ()) => Show (IntArgsMonoSet k)

empty :: IntArgsMonoSet k
empty = Set Map.empty

singleton :: MonoMapKey k => Int -> k -> IntArgsMonoSet k
singleton i k = Set (Map.singleton i k ())

insert :: MonoMapKey k => Int -> k -> IntArgsMonoSet k -> IntArgsMonoSet k
insert i k = coerce (Map.insert i k ())

delete :: MonoMapKey k => Int -> k -> IntArgsMonoSet k -> IntArgsMonoSet k
delete = coerce Map.delete

member :: MonoMapKey k => Int -> k -> IntArgsMonoSet k -> Bool
member i k (Set m) = isJust (Map.lookup i k m)

difference :: MonoMapKey k => IntArgsMonoSet k -> IntArgsMonoSet k -> IntArgsMonoSet k
difference = coerce Map.difference

toList :: MonoMapKey k => IntArgsMonoSet k -> [(Int, k)]
toList (Set m) = Map.keys m

highestPriorityNodes :: MonoMapKey k => IntArgsMonoSet k -> [(Int, k)]
highestPriorityNodes = coerce Map.highestPriorityNodes
