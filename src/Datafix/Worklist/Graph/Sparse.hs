{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- |
-- Module      :  Datafix.Worklist.Graph
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Sparse data-flow graph representation based on 'Data.IntMap.Strict.IntMap'.

module Datafix.Worklist.Graph.Sparse
  ( Ref
  , newRef
  ) where

import           Control.Monad                    (forM_)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.IORef
import           Data.Maybe                       (fromMaybe)
import           Datafix.IntArgsMonoMap           (IntArgsMonoMap)
import qualified Datafix.IntArgsMonoMap           as IntArgsMonoMap
import qualified Datafix.IntArgsMonoSet           as IntArgsMonoSet
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph

-- | Models a data-flow graph as a map from 'Node's to
-- associated points of their transfer function.
type Graph domain
  = IntArgsMonoMap (Products (ParamTypes domain)) (PointInfo domain)

-- | Reference to a sparse data-flow graph representation.
newtype Ref domain =
  Ref (IORef (Graph domain))

-- | Allocates a new sparse graph 'Ref'.
newRef :: IO (Ref domain)
newRef = Ref <$> newIORef IntArgsMonoMap.empty

fromState :: State (Graph domain) a -> ReaderT (Ref domain) IO a
fromState st = do
  Ref ref <- ask
  g <- lift (readIORef ref)
  let (a, g') = runState st g
  g' `seq` lift (writeIORef ref g')
  pure a
{-# INLINE fromState #-}

instance GraphRef Ref where
  updatePoint node args val refs = fromState $ do
    -- if we are lucky (e.g. no refs changed), we get away with one map access
    -- first update 'node's PointInfo
    let freshInfo = emptyPointInfo
          { value = Just val
          , references = refs
          , iterations = 1
          }
    let merger _ _ new old = new
          { referrers = referrers old
          , iterations = iterations old + 1
          }
    oldInfo <- fromMaybe emptyPointInfo <$>
      state (IntArgsMonoMap.insertLookupWithKey merger node args freshInfo)

    -- Now compute the diff of changed references
    let diff = computeDiff (references oldInfo) refs

    -- finally register/unregister at all references as referrer.
    let updater f (depNode, depArgs) = modify' $
          IntArgsMonoMap.insertWith (const f) depNode depArgs (f emptyPointInfo)
    let addReferrer ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
    let removeReferrer ni = ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
    forM_ (IntArgsMonoSet.toList (added diff)) (updater addReferrer)
    forM_ (IntArgsMonoSet.toList (removed diff)) (updater removeReferrer)

    return oldInfo
  {-# INLINE updatePoint #-}

  lookup node args = do
    Ref ref <- ask
    IntArgsMonoMap.lookup node args <$> lift (readIORef ref)
  {-# INLINE lookup #-}

  lookupLT node args = do
    Ref ref <- ask
    IntArgsMonoMap.lookupLT node args <$> lift (readIORef ref)
  {-# INLINE lookupLT #-}
