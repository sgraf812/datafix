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
import           Data.IORef
import           Data.Maybe                       (fromMaybe)
import           Datafix.Description              (ChangeDetector, Domain,
                                                   Node (..), TransferFunction)
import           Datafix.IntArgsMonoSet           (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet           as IntArgsMonoSet
import           Datafix.MonoMap                  (MonoMap, MonoMapKey)
import qualified Datafix.MonoMap                  as MonoMap
import           Datafix.Utils.GrowableVector     (GrowableVector)
import qualified Datafix.Utils.GrowableVector     as GrowableVector
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

data NodeInfo m
  = NodeInfo
  { changeDetector :: !(ChangeDetector m)
  , transfer       :: !(TransferFunction m)
  , points         :: !(PointMap (Domain m))
  }

newNodeInfo
  :: MonoMapKey (Products (ParamTypes (Domain m)))
  => ChangeDetector m
  -> TransferFunction m
  -> NodeInfo m
newNodeInfo cd tf = NodeInfo cd tf MonoMap.empty

-- | A dense data-flow graph representation.
newtype Graph m
  = Graph (IORef (GrowableVector (PrimState IO) (NodeInfo m)))

-- | Allocates a new dense graph 'Graph'.
new :: MonoMapKey (Products (ParamTypes (Domain m))) => IO (Graph m)
new = Graph <$> (GrowableVector.new 1024 >>= newIORef)

allocateNode
  :: MonoMapKey (Products (ParamTypes (Domain m)))
  => ChangeDetector m
  -> TransferFunction m
  -> ReaderT (Graph m) IO Int
allocateNode cd tf = ReaderT $ \(Graph ref) -> do
  vec <- readIORef ref
  vec' <- GrowableVector.pushBack vec (newNodeInfo cd tf)
  writeIORef ref vec'
  return (GrowableVector.length vec')
{-# INLINE allocateNode #-}

zoomPoints :: Int -> State (PointMap (Domain m)) a -> ReaderT (Graph m) IO a
zoomPoints node s = ReaderT $ \(Graph ref) -> do
  vec <- readIORef ref
  ni <- GrowableVector.read vec node
  let (ret, points') = runState s (points ni)
  points' `seq` GrowableVector.write vec node ni { points = points' }
  return ret
{-# INLINE zoomPoints #-}

updatePoint
  :: MonoMapKey (Products (ParamTypes (Domain m)))
  => Node
  -> Products (ParamTypes (Domain m))
  -> ReturnType (Domain m)
  -> IntArgsMonoSet (Products (ParamTypes (Domain m)))
  -> ReaderT (Graph m) IO (PointInfo (Domain m))
updatePoint (Node node) args val refs = do
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

lookupNode :: MonoMapKey (Products (ParamTypes (Domain m))) => Int -> ReaderT (Graph m) IO (NodeInfo m)
lookupNode node = ReaderT $ \(Graph ref) -> do
  graph <- readIORef ref
  GrowableVector.read graph node
{-# INLINE lookupNode #-}

lookup :: MonoMapKey (Products (ParamTypes (Domain m))) => Int -> Products (ParamTypes (Domain m)) -> ReaderT (Graph m) IO (Maybe (PointInfo (Domain m)))
lookup node args = MonoMap.lookup args . points <$> lookupNode node
{-# INLINE lookup #-}

lookupLT :: MonoMapKey (Products (ParamTypes (Domain m))) => Int -> Products (ParamTypes (Domain m)) -> ReaderT (Graph m) IO [(Products (ParamTypes (Domain m)), PointInfo (Domain m))]
lookupLT node args = ReaderT $ \(Graph ref) -> do
  graph <- readIORef ref
  MonoMap.lookupLT args . points <$> GrowableVector.read graph node
{-# INLINE lookupLT #-}
