{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Datafix.Worklist.Graph.Dense where

import           Control.Monad                    (forM_)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.Maybe                       (fromMaybe)
import           Data.Vector.Mutable              (IOVector)
import qualified Data.Vector.Mutable              as V
import qualified Datafix.IntArgsMonoSet           as IntArgsMonoSet
import           Datafix.MonoMap                  (MonoMap, MonoMapKey)
import qualified Datafix.MonoMap                  as MonoMap
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph

type PointMap domain
  = MonoMap (Products (Domains domain)) (NodeInfo domain)

newtype Ref domain
  = Ref (IOVector (PointMap domain))

newRef :: MonoMapKey (Products (Domains domain)) => Int -> IO (Ref domain)
newRef size = Ref <$> V.replicate size MonoMap.empty

zoomNode :: Int -> State (PointMap domain) a -> ReaderT (Ref domain) IO a
zoomNode node s = do
  Ref graph <- ask
  points <- lift (V.read graph node)
  let (ret, points') = runState s points
  points' `seq` lift (V.write graph node points')
  return ret
{-# INLINE zoomNode #-}

instance GraphRef Ref where
  updatePoint node args val refs = do
    -- if we are lucky (e.g. no refs changed), we get away with one map access
    -- first update `node`s NodeInfo
    let freshInfo = emptyNodeInfo
          { value = Just val
          , references = refs
          , iterations = 1
          }
    let merger _ new old = new
          { referrers = referrers old
          , iterations = iterations old + 1
          }

    oldInfo <- fmap (fromMaybe emptyNodeInfo) $ zoomNode node $ state $
      MonoMap.insertLookupWithKey merger args freshInfo

    -- Now compute the diff of changed references
    let diff = computeDiff (references oldInfo) refs

    -- finally register/unregister at all references as referrer.
    let updater f (depNode, depArgs) = zoomNode depNode $ modify' $
          MonoMap.insertWith (const f) depArgs (f emptyNodeInfo)
    let addReferrer ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
    let removeReferrer ni = ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
    forM_ (IntArgsMonoSet.toList (added diff)) (updater addReferrer)
    forM_ (IntArgsMonoSet.toList (removed diff)) (updater removeReferrer)

    return oldInfo
  {-# INLINE updatePoint #-}

  lookup node args = ReaderT $ \(Ref graph) ->
    MonoMap.lookup args <$> V.read graph node
  {-# INLINE lookup #-}

  lookupLT node args = ReaderT $ \(Ref graph) ->
    MonoMap.lookupLT args <$> V.read graph node
  {-# INLINE lookupLT #-}
