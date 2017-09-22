{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Datafix.Worklist.Internal where

import           Algebra.Lattice
import           Control.Monad              (forM_)
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Proxy                 (Proxy (..))
import           Data.STRef
import           Datafix
import           Datafix.IntArgsMonoMap     (IntArgsMonoMap)
import qualified Datafix.IntArgsMonoMap     as IntArgsMonoMap
import           Datafix.IntArgsMonoSet     (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet     as IntArgsMonoSet
import           Datafix.MonoMap            (MonoMapKey)
import           Datafix.Utils.TypeLevel

newtype DependencyM s domain a
  = DM (ReaderT (Env s domain) (ST s) a)
  deriving (Functor, Applicative, Monad)

data NodeInfo domain
  = NodeInfo
  { value      :: !(Maybe (CoDomain domain)) -- ^ the value at this node. Can be Nothing only when a loop was detected
  , references :: !(IntArgsMonoSet (Products (Domains domain)))      -- ^ nodes this value depends on
  , referrers  :: !(IntArgsMonoSet (Products (Domains domain)))      -- ^ nodes depending on this value
  }

deriving instance (Eq (CoDomain domain), Eq (IntArgsMonoSet (Products (Domains domain)))) => Eq (NodeInfo domain)
deriving instance (Show (CoDomain domain), Show (IntArgsMonoSet (Products (Domains domain)))) => Show (NodeInfo domain)

emptyNodeInfo :: NodeInfo domain
emptyNodeInfo = NodeInfo Nothing IntArgsMonoSet.empty IntArgsMonoSet.empty
{-# INLINE emptyNodeInfo #-}

type Graph domain
  = IntArgsMonoMap (Products (Domains domain)) (NodeInfo domain)

data Env s domain
  = Env
  { problem   :: !(DataFlowProblem (DependencyM s domain))
  , callStack :: !(IntArgsMonoSet (Products (Domains domain)))
  , current   :: !(Maybe (Int, Products (Domains domain)))
  , graph     :: !(STRef s (Graph domain))
  , unstable  :: !(STRef s (IntArgsMonoSet (Products (Domains domain)))) -- unstable nodes and their changed references
  }

initialEnv
  :: IntArgsMonoSet (Products (Domains domain))
  -> DataFlowProblem (DependencyM s domain)
  -> ST s (Env s domain)
initialEnv unstable_ fw =
  Env fw IntArgsMonoSet.empty Nothing
    <$> newSTRef IntArgsMonoMap.empty
    <*> newSTRef unstable_
{-# INLINE initialEnv #-}

type Datafixable m =
  ( Currying (Domains (Domain m)) (CoDomain (Domain m))
  , Currying (Domains (Domain m)) (m (CoDomain (Domain m)))
  , Currying (Domains (Domain m)) (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)
  , MonoMapKey (Products (Domains (Domain m)))
  , BoundedJoinSemiLattice (CoDomain (Domain m))
  )

instance Datafixable (DependencyM s domain) => MonadDependency (DependencyM s domain) where
  type Domain (DependencyM s domain) = domain
  dependOn = dependOn'

zoomGraph :: State (Graph domain) a -> ReaderT (Env s domain) (ST s) a
zoomGraph modifyGraph = do
  ref <- asks graph
  g <- lift $ readSTRef ref
  let (res, g') = runState modifyGraph g
  lift $ writeSTRef ref g'
  return res
{-# INLINE zoomGraph #-}

zoomUnstable
  :: State (IntArgsMonoSet (Products (Domains domain))) a
  -> ReaderT (Env s domain) (ST s) a
zoomUnstable modifyUnstable = do
  ref <- asks unstable
  uns <- lift $ readSTRef ref
  let (res, uns') = runState modifyUnstable uns
  lift $ writeSTRef ref uns'
  return res
{-# INLINE zoomUnstable #-}

enqueueUnstable
  :: k ~ Products (Domains domain)
  => MonoMapKey k
  => Int -> k -> ReaderT (Env s domain) (ST s) ()
enqueueUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.insert i k))
{-# INLINE enqueueUnstable #-}

deleteUnstable
  :: k ~ Products (Domains domain)
  => MonoMapKey k
  => Int -> k -> ReaderT (Env s domain) (ST s) ()
deleteUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.delete i k))
{-# INLINE deleteUnstable #-}

highestPriorityUnstableNode
  :: k ~ Products (Domains domain)
  => MonoMapKey k
  => ReaderT (Env s domain) (ST s) (Maybe (Int, k))
highestPriorityUnstableNode = zoomUnstable $
  listToMaybe . IntArgsMonoSet.highestPriorityNodes <$> get
{-# INLINE highestPriorityUnstableNode #-}

clearReferences
  :: MonoMapKey (Products (Domains domain))
  => Int
  -> Products (Domains domain)
  -> ReaderT (Env s domain) (ST s) (NodeInfo domain)
clearReferences node args = zoomGraph $ do
  let merger _ _ _ old = old { references = IntArgsMonoSet.empty }
  oldInfo <- fromMaybe emptyNodeInfo <$>
    state (IntArgsMonoMap.insertLookupWithKey merger node args emptyNodeInfo)
  let deleteReferrer ni =
        ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
  forM_ (IntArgsMonoSet.toList (references oldInfo)) $ \(depNode, depArgs) ->
    modify' (IntArgsMonoMap.adjust deleteReferrer depNode depArgs)
  return oldInfo
{-# INLINE clearReferences #-}

updateNodeValue
  :: MonoMapKey (Products (Domains domain))
  => Int
  -> Products (Domains domain)
  -> CoDomain domain
  -> ReaderT (Env s domain) (ST s) (NodeInfo domain)
updateNodeValue node args val = zoomGraph $ do
  let updater _ _ ni = Just ni { value = Just val }
  oldInfo <- fromMaybe (error "There should be an entry when this is called") <$>
    state (IntArgsMonoMap.updateLookupWithKey updater node args)
  return oldInfo { value = Just val }
{-# INLINE updateNodeValue #-}

withCall
  :: Datafixable (DependencyM s domain)
  => Int
  -> Products (Domains domain)
  -> ReaderT (Env s domain) (ST s) a
  -> ReaderT (Env s domain) (ST s) a
withCall node args = local $ \env -> env
  { callStack = IntArgsMonoSet.insert node args (callStack env)
  , current = Just (node, args)
  }
{-# INLINE withCall #-}

recompute
  :: forall domain dom cod s
   . dom ~ Domains domain
  => cod ~ CoDomain domain
  => Datafixable (DependencyM s domain)
  => Int -> Products dom -> ReaderT (Env s domain) (ST s) cod
recompute node args = withCall node args $ do
  deleteUnstable node args
  maybeOldVal <- value <$> clearReferences node args
  prob <- asks problem
  let dom = Proxy :: Proxy dom
  let depm = Proxy :: Proxy (DependencyM s domain cod)
  let eq = Proxy :: Proxy (cod -> cod -> Bool)
  let node' = Node node
  let DM iterate' = uncurrys dom depm (transfer prob node') args
  let detectChange' = uncurrys dom eq (detectChange prob node') args
  newVal <- iterate'
  newInfo <- updateNodeValue node args newVal
  case maybeOldVal of
    Just oldVal | not (detectChange' oldVal newVal) ->
      return ()
    _ ->
      forM_ (IntArgsMonoSet.toList (referrers newInfo)) (uncurry enqueueUnstable)
  return newVal
{-# INLINE recompute #-}

addReference
  :: MonoMapKey (Products (Domains domain))
  => Int
  -> Products (Domains domain)
  -> Int
  -> Products (Domains domain)
  -> ReaderT (Env s domain) (ST s) ()
addReference node args depNode depArgs = zoomGraph $ do
  let adjustReferences ni = ni { references = IntArgsMonoSet.insert depNode depArgs (references ni) }
  modify' (IntArgsMonoMap.adjust adjustReferences node args)
  let adjustReferrers ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
  modify' (IntArgsMonoMap.adjust adjustReferrers depNode depArgs)
{-# INLINE addReference #-}

dependOn'
  :: forall domain s
   . Datafixable (DependencyM s domain)
  => Proxy (DependencyM s domain) -> Node -> TransferFunction (DependencyM s domain) domain
dependOn' _ (Node node) = currys dom cod impl
  where
    dom = Proxy :: Proxy (Domains domain)
    cod = Proxy :: Proxy (DependencyM s domain (CoDomain domain))
    impl args = DM $ do
      cycleDetected <- IntArgsMonoSet.member node args <$> asks callStack
      isStable <- zoomUnstable $
        not . IntArgsMonoSet.member node args <$> get
      maybeNodeInfo <- zoomGraph $
        IntArgsMonoMap.lookup node args <$> get
      val <-
        case maybeNodeInfo >>= value of
          -- 'value' can only be 'Nothing' if there was a 'cycleDetected':
          -- Otherwise, the node wasn't part of the call stack and thus will either
          -- have a 'value' assigned or will not have been discovered at all.
          Nothing | cycleDetected ->
            -- Somewhere in an outer activation record we already compute this one.
            -- We don't recurse again and just return 'bottom'.
            -- Otherwise, 'recompute' will immediately add a 'NodeInfo' before
            -- any calls to 'dependOn' for a cycle to even be possible.
            return bottom
          Just val | isStable || cycleDetected ->
            -- No brainer
            return val
          maybeVal ->
            -- No cycle && (unstable || undiscovered). Apply one of the schemes
            -- outlined in
            -- https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like
            scheme2 maybeVal node args
      -- save that we depend on this value
      (curNode, curArgs) <- fromMaybe (error "`dependOn` can only be called in an activation record") <$> asks current
      addReference curNode curArgs node args
      return val
{-# INLINE dependOn' #-}

scheme1, scheme2, scheme3
  :: Datafixable (DependencyM s domain)
  => Maybe (CoDomain domain)
  -> Int
  -> Products (Domains domain)
  -> ReaderT (Env s domain) (ST s) (CoDomain domain)
{-# INLINE scheme1 #-}
{-# INLINE scheme2 #-}
{-# INLINE scheme3 #-}

-- | scheme 1 (see https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like).
--
-- Let the worklist algorithm figure things out.
scheme1 _ _ _ = return bottom

-- | scheme 2 (see https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like).
--
-- Descend into \(\bot\) nodes when there is no cycle to discover the set of
-- reachable nodes as quick as possible.
-- Do *not* descend into unstable, non-\(\bot\) nodes.
scheme2 maybeVal node args =
  case maybeVal of
    Nothing ->
      -- Depth-first discovery of reachable nodes
      recompute node args
    Just val ->
      -- It is unclear if this really is beneficial:
      -- We don't discover any new nodes and should rather
      -- rely on the ordering in the worklist.
      return val

-- | scheme 3 (see https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like).
--
-- Descend into unstable nodes when there is no cycle.
-- It's unclear if this leads to better performance than 'scheme2'.
scheme3 _ = recompute

whileJust_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ cond action = go
  where
    go = cond >>= \m -> case m of
      Nothing -> return ()
      Just a  -> action a >> go
{-# INLINE whileJust_ #-}

work :: Datafixable (DependencyM s domain) => ReaderT (Env s domain) (ST s) ()
work = whileJust_ highestPriorityUnstableNode (uncurry recompute)
{-# INLINE work #-}

fixProblem
  :: forall domain
   . MonoMapKey (Products (Domains domain))
  => Currying (Domains domain) (CoDomain domain)
  => (forall s r. (Datafixable (DependencyM s domain) => r) -> r)
  -> (forall s. DataFlowProblem (DependencyM s domain))
  -> Node
  -> Arrows (Domains domain) (CoDomain domain)
fixProblem withDatafixable prob (Node node) = currys (Proxy :: Proxy (Domains domain)) (Proxy :: Proxy (CoDomain domain)) impl
  where
    impl args
      = fromMaybe (error "Broken invariant: The root node has no value")
      . (>>= value)
      . IntArgsMonoMap.lookup node args
      $ runST (runProblem args)
    runProblem :: forall s. Products (Domains domain) -> ST s (Graph domain)
    runProblem args = do
      env <- initialEnv (IntArgsMonoSet.singleton node args) prob
      runReaderT (withDatafixable @s work >> asks graph >>= lift . readSTRef) env
{-# INLINE fixProblem #-}