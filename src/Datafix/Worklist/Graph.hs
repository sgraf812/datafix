{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Datafix.Worklist.Graph where

import           Control.Monad.Trans.Reader
import           Datafix.IntArgsMonoSet     (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet     as IntArgsMonoSet
import           Datafix.MonoMap            (MonoMapKey)
import           Datafix.Utils.TypeLevel

data NodeInfo domain
  = NodeInfo
  { value      :: !(Maybe (CoDomain domain))
  -- ^ The value at this point. Can be 'Nothing' only when a loop was detected.
  , references :: !(IntArgsMonoSet (Products (Domains domain)))
  -- ^ Points this value depends on.
  , referrers  :: !(IntArgsMonoSet (Products (Domains domain)))
  -- ^ Points depending on this value.
  , iterations :: !Int
  -- ^ The number of times this point has been updated through calls to
  -- 'updateNodeValue'.
  }

deriving instance (Eq (CoDomain domain), Eq (IntArgsMonoSet (Products (Domains domain)))) => Eq (NodeInfo domain)
deriving instance (Show (CoDomain domain), Show (IntArgsMonoSet (Products (Domains domain)))) => Show (NodeInfo domain)

emptyNodeInfo :: NodeInfo domain
emptyNodeInfo = NodeInfo Nothing IntArgsMonoSet.empty IntArgsMonoSet.empty 0
{-# INLINE emptyNodeInfo #-}

class GraphRef (ref :: * -> *) where
  clearReferences :: MonoMapKey (Products (Domains domain)) => Int -> Products (Domains domain) -> ReaderT (ref domain) IO (NodeInfo domain)
  updateNodeValue :: MonoMapKey (Products (Domains domain)) => Int -> Products (Domains domain) -> CoDomain domain -> ReaderT (ref domain) IO (NodeInfo domain)
  addReference :: MonoMapKey (Products (Domains domain)) => Int -> Products (Domains domain) -> Int -> Products (Domains domain) -> ReaderT (ref domain) IO ()
  lookup :: MonoMapKey (Products (Domains domain)) => Int -> Products (Domains domain) -> ReaderT (ref domain) IO (Maybe (NodeInfo domain))
  lookupLT :: MonoMapKey (Products (Domains domain)) => Int -> Products (Domains domain) -> ReaderT (ref domain) IO [(Products (Domains domain), NodeInfo domain)]
