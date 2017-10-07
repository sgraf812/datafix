{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Datafix.Worklist.Graph.Sparse where

import           Control.Monad              (forM_)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Maybe                 (fromMaybe)
import           Datafix.IntArgsMonoMap     (IntArgsMonoMap)
import qualified Datafix.IntArgsMonoMap     as IntArgsMonoMap
import           Datafix.IntArgsMonoSet     (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet     as IntArgsMonoSet
import           Datafix.MonoMap            (MonoMapKey)
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph
import           Debug.Trace

type Graph domain
  = IntArgsMonoMap (Products (Domains domain)) (NodeInfo domain)

newtype Ref domain =
  Ref (IORef (Graph domain))

newRef :: IO (Ref domain)
newRef = Ref <$> newIORef IntArgsMonoMap.empty

fromState :: State (Graph domain) a -> ReaderT (Ref domain) IO a
fromState st = do
  Ref ref <- ask
  g <- lift (readIORef ref)
  let (a, g') = runState st g
  lift (writeIORef ref g')
  pure a
{-# INLINE fromState #-}

instance GraphRef Ref where
  updatePoint node args val refs = fromState $ do
    -- if we are lucky (e.g. no refs changed), we get away with one map access
    -- first update `node`s NodeInfo
    let freshInfo = emptyNodeInfo
          { value = Just (val)
          , references = refs
          , iterations = 1
          }
    let merger _ _ new old = new
          { referrers = referrers old
          , iterations = iterations old + 1
          }
    oldInfo <- fromMaybe emptyNodeInfo <$>
      state (IntArgsMonoMap.insertLookupWithKey merger node args freshInfo)

    -- Now compute the diff of changed references
    let diff = computeDiff (references oldInfo) refs

    -- finally register/unregister at all references as referrer.
    let updater f (depNode, depArgs) = modify' $
          IntArgsMonoMap.insertWith (const f) depNode depArgs (f emptyNodeInfo)
    let addReferrer ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
    let removeReferrer ni = ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
    forM_ (IntArgsMonoSet.toList (added diff)) (updater addReferrer)
    forM_ (IntArgsMonoSet.toList (removed diff)) (updater removeReferrer)

    return (oldInfo)
  {-# INLINE updatePoint #-}

  lookup node args = fromState $ IntArgsMonoMap.lookup node args <$> get
  {-# INLINE lookup #-}

  lookupLT node args = fromState $ IntArgsMonoMap.lookupLT node args <$> get
  {-# INLINE lookupLT #-}
