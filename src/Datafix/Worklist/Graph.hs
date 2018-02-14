{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Datafix.Worklist.Graph
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Abstracts over the representation of the data-flow graph.
--
-- The contents of this module are more or less internal to the
-- "Datafix.Worklist" implementation.

module Datafix.Worklist.Graph where

import           Control.Monad.Trans.Reader
import           Datafix.IntArgsMonoSet     (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet     as IntArgsMonoSet
import           Datafix.MonoMap            (MonoMapKey)
import           Datafix.Utils.TypeLevel

-- | The data associated with each point in the transfer function of a data-flow
-- 'Node'.
data PointInfo domain
  = PointInfo
  { value      :: !(Maybe (ReturnType domain))
  -- ^ The value at this point. Can be 'Nothing' only when a loop was detected.
  , references :: !(IntArgsMonoSet (Products (ParamTypes domain)))
  -- ^ Points this point of the transfer function depends on.
  , referrers  :: !(IntArgsMonoSet (Products (ParamTypes domain)))
  -- ^ Points depending on this point.
  , iterations :: !Int
  -- ^ The number of times this point has been updated through calls to
  -- 'updateNodeValue'.
  }

deriving instance (Eq (ReturnType domain), Eq (IntArgsMonoSet (Products (ParamTypes domain)))) => Eq (PointInfo domain)
deriving instance (Show (ReturnType domain), Show (IntArgsMonoSet (Products (ParamTypes domain)))) => Show (PointInfo domain)

-- | The default 'PointInfo'.
emptyPointInfo :: PointInfo domain
emptyPointInfo = PointInfo Nothing IntArgsMonoSet.empty IntArgsMonoSet.empty 0
{-# INLINE emptyPointInfo #-}

-- | Diff between two 'IntArgsMonoSet's.
data Diff a
  = Diff
  { added   :: !(IntArgsMonoSet a)
  , removed :: !(IntArgsMonoSet a)
  }

-- | Computes the diff between two 'IntArgsMonoSet's.
computeDiff :: MonoMapKey k => IntArgsMonoSet k -> IntArgsMonoSet k -> Diff k
computeDiff a b =
  Diff (IntArgsMonoSet.difference b a) (IntArgsMonoSet.difference a b)

-- | Abstracts over the concrete representation of the data-flow graph.
--
-- There are two instances: The default 'Datafix.Graph.Sparse.Ref'
-- for sparse graphs based on an 'IntMap' and 'Datafix.Graph.Dense.Ref' for
-- the dense case, storing the 'Node' mapping in a 'Data.IOVector'.
class GraphRef (ref :: * -> *) where
  updatePoint :: MonoMapKey (Products (ParamTypes domain)) => Int -> Products (ParamTypes domain) -> ReturnType domain -> IntArgsMonoSet (Products (ParamTypes domain)) -> ReaderT (ref domain) IO (PointInfo domain)
  lookup :: MonoMapKey (Products (ParamTypes domain)) => Int -> Products (ParamTypes domain) -> ReaderT (ref domain) IO (Maybe (PointInfo domain))
  lookupLT :: MonoMapKey (Products (ParamTypes domain)) => Int -> Products (ParamTypes domain) -> ReaderT (ref domain) IO [(Products (ParamTypes domain), PointInfo domain)]
