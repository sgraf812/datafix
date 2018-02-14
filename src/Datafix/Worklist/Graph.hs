{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Datafix.Worklist.Graph
-- Copyright   :  (c) Sebastian Graf 2017
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Abstracts over the representation of the data-flow graph.
--
-- The contents of this module are more or less internal to the
-- "Datafix.Worklist" implementation.

module Datafix.Worklist.Graph where

import           Control.Monad                    (forM_)
import           Control.Monad.Primitive          (PrimState)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.Maybe                       (fromMaybe)
import           Data.Primitive.Array
import           Datafix.Description              (ChangeDetector, Domain,
                                                   LiftFunc)
import           Datafix.IntArgsMonoSet           (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet           as IntArgsMonoSet
import           Datafix.MonoMap                  (MonoMap, MonoMapKey)
import qualified Datafix.MonoMap                  as MonoMap
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

-- | Models the points of a transfer function of a single
-- data-flow 'Node'.
type PointMap domain
  = MonoMap (Products (ParamTypes domain)) (PointInfo domain)

-- | A dense data-flow graph representation.
newtype Graph domain
  = Graph (MutableArray (PrimState IO) (PointMap domain))

-- | Allocates a new dense graph 'Graph'.
new :: MonoMapKey (Products (ParamTypes domain)) => Int -> IO (Graph domain)
new n = Graph <$> newArray n MonoMap.empty

zoomPoints :: Int -> State (PointMap domain) a -> ReaderT (Graph domain) IO a
zoomPoints node s = ReaderT $ \(Graph graph) -> do
  points <- readArray graph node
  let (ret, points') = runState s points
  points' `seq` writeArray graph node points'
  return ret
{-# INLINE zoomPoints #-}

updatePoint
  :: MonoMapKey (Products (ParamTypes domain))
  => Int
  -> Products (ParamTypes domain)
  -> ReturnType domain
  -> IntArgsMonoSet (Products (ParamTypes domain))
  -> ReaderT (Graph domain) IO (PointInfo domain)
updatePoint node args val refs = do
  -- if we are lucky (e.g. no refs changed), we get away with one map access
  -- first update `node`s PointInfo
  let freshInfo = emptyPointInfo
        { value = Just val
        , references = refs
        , iterations = 1
        }
  let merger _ new_ old = new_
        { referrers = referrers old
        , iterations = iterations old + 1
        }

  oldInfo <- fmap (fromMaybe emptyPointInfo) $ zoomPoints node $ state $
    MonoMap.insertLookupWithKey merger args freshInfo

  -- Now compute the diff of changed references
  let diff = computeDiff (references oldInfo) refs

  -- finally register/unregister at all references as referrer.
  let updater f (depNode, depArgs) = zoomPoints depNode $ modify' $
        MonoMap.insertWith (const f) depArgs (f emptyPointInfo)
  let addReferrer ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
  let removeReferrer ni = ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
  forM_ (IntArgsMonoSet.toList (added diff)) (updater addReferrer)
  forM_ (IntArgsMonoSet.toList (removed diff)) (updater removeReferrer)

  return oldInfo
{-# INLINE updatePoint #-}

lookup :: MonoMapKey (Products (ParamTypes domain)) => Int -> Products (ParamTypes domain) -> ReaderT (Graph domain) IO (Maybe (PointInfo domain))
lookup node args = ReaderT $ \(Graph graph) -> MonoMap.lookup args <$> readArray graph node
{-# INLINE lookup #-}

lookupLT :: MonoMapKey (Products (ParamTypes domain)) => Int -> Products (ParamTypes domain) -> ReaderT (Graph domain) IO [(Products (ParamTypes domain), PointInfo domain)]
lookupLT node args = ReaderT $ \(Graph graph) -> MonoMap.lookupLT args <$> readArray graph node
{-# INLINE lookupLT #-}
