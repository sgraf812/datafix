{-# LANGUAGE FlexibleContexts #-}

module Datafix.Worklist.Graph.Dense where

import           Control.Monad              (forM_)
import           Control.Monad.Trans.Reader
import           Data.Maybe                 (fromMaybe)
import           Data.Vector.Mutable        (IOVector)
import qualified Data.Vector.Mutable        as V
import qualified Datafix.IntArgsMonoSet     as IntArgsMonoSet
import           Datafix.MonoMap            (MonoMap, MonoMapKey)
import qualified Datafix.MonoMap            as MonoMap
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph

type PointMap domain
  = MonoMap (Products (Domains domain)) (NodeInfo domain)

newtype Ref domain =
  Ref (IOVector (PointMap domain))

newRef :: MonoMapKey (Products (Domains domain)) => Int -> IO (Ref domain)
newRef size = Ref <$> V.replicate size MonoMap.empty

seqWrite :: IOVector v -> Int -> v -> IO ()
seqWrite arr i v = seq v (V.write arr i v)

instance GraphRef Ref where
  clearReferences node args = ReaderT $ \(Ref graph) -> do
    points <- V.read graph node
    let merger _ _ old = old { references = IntArgsMonoSet.empty }
    let (maybeOldInfo, points') = MonoMap.insertLookupWithKey merger args emptyNodeInfo points
    let oldInfo = fromMaybe emptyNodeInfo maybeOldInfo
    seqWrite graph node points'
    let deleteReferrer ni =
          ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
    forM_ (IntArgsMonoSet.toList (references oldInfo)) $ \(depNode, depArgs) -> do
      depPoints <- V.read graph depNode
      seqWrite graph depNode (MonoMap.adjust deleteReferrer depArgs depPoints)
    return oldInfo
  {-# INLINE clearReferences #-}

  updateNodeValue node args val = ReaderT $ \(Ref graph) -> do
    points <- V.read graph node
    let update ni = ni { value = Just val, iterations = iterations ni + 1 }
    let updater _ = Just . update
    let (maybeOldInfo, points') = MonoMap.updateLookupWithKey updater args points
    seqWrite graph node points'
    let oldInfo = fromMaybe (error "There should be an entry when this is called") maybeOldInfo
    return (update oldInfo)
  {-# INLINE updateNodeValue #-}

  addReference fromNode fromArgs toNode toArgs = ReaderT $ \(Ref graph) -> do
    fromPoints <- V.read graph fromNode
    let adjustReferences ni = ni { references = IntArgsMonoSet.insert toNode toArgs (references ni) }
    seqWrite graph fromNode (MonoMap.adjust adjustReferences fromArgs fromPoints)
    toPoints <- V.read graph toNode
    let adjustReferrers ni = ni { referrers = IntArgsMonoSet.insert fromNode fromArgs (referrers ni) }
    seqWrite graph toNode (MonoMap.adjust adjustReferrers toArgs toPoints)
  {-# INLINE addReference #-}

  lookup node args = ReaderT $ \(Ref graph) ->
    MonoMap.lookup args <$> V.read graph node
  {-# INLINE lookup #-}

  lookupLT node args = ReaderT $ \(Ref graph) ->
    MonoMap.lookupLT args <$> V.read graph node
  {-# INLINE lookupLT #-}
