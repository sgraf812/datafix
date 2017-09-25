{-# LANGUAGE FlexibleContexts #-}

module Datafix.Worklist.Graph.Sparse where

import           Control.Monad              (forM_)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Maybe                 (fromMaybe)
import           Datafix.IntArgsMonoMap     (IntArgsMonoMap)
import qualified Datafix.IntArgsMonoMap     as IntArgsMonoMap
import qualified Datafix.IntArgsMonoSet     as IntArgsMonoSet
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph

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

instance GraphRef Ref where
  clearReferences node args = fromState $ do
    let merger _ _ _ old = old { references = IntArgsMonoSet.empty }
    oldInfo <- fromMaybe emptyNodeInfo <$>
      state (IntArgsMonoMap.insertLookupWithKey merger node args emptyNodeInfo)
    let deleteReferrer ni =
          ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
    forM_ (IntArgsMonoSet.toList (references oldInfo)) $ \(depNode, depArgs) ->
      modify' (IntArgsMonoMap.adjust deleteReferrer depNode depArgs)
    return oldInfo
  {-# INLINE clearReferences #-}

  updateNodeValue node args val = fromState $ do
    let updater _ _ ni = Just ni { value = Just val }
    oldInfo <- fromMaybe (error "There should be an entry when this is called") <$>
      state (IntArgsMonoMap.updateLookupWithKey updater node args)
    return oldInfo { value = Just val }
  {-# INLINE updateNodeValue #-}

  addReference node args depNode depArgs = fromState $ do
    let adjustReferences ni = ni { references = IntArgsMonoSet.insert depNode depArgs (references ni) }
    modify' (IntArgsMonoMap.adjust adjustReferences node args)
    let adjustReferrers ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
    modify' (IntArgsMonoMap.adjust adjustReferrers depNode depArgs)
  {-# INLINE addReference #-}

  lookup node args = fromState $ IntArgsMonoMap.lookup node args <$> get
  {-# INLINE lookup #-}
