{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fprof-auto #-}

module Datafix
  ( GraphNode (..)
  , DependencyM
  , TransferFunction
  , Datafixable
  , dependOn
  , unsafePeekValue
  , ChangeDetector
  , eqChangeDetector
  , alwaysChangeDetector
  , DataFlowProblem (DFP)
  , fixProblem
  ) where

import           Algebra.Lattice
import           Control.Monad                    (forM_, when, (<=<))
import           Control.Monad.Trans.State.Strict
import           Data.Maybe                       (fromMaybe, listToMaybe)
import           Data.Proxy                       (Proxy (..))
import           Datafix.IntArgsMonoMap           (IntArgsMonoMap)
import qualified Datafix.IntArgsMonoMap           as IntArgsMonoMap
import           Datafix.IntArgsMonoSet           (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet           as IntArgsMonoSet
import           Datafix.MonoMap                  (MonoMapKey)
import           Datafix.Utils.TypeLevel

-- | This is the type we use to index nodes in the data-flow graph.
--
-- The connection between syntactic things (e.g. 'Id's) and 'GraphNode's is
-- made implicitly in code in analysis templates through an appropriate
-- allocation mechanism as in 'NodeAllocator'
newtype GraphNode
  = GraphNode Int
  deriving (Eq, Ord, Show)

newtype DependencyM lattice a
  = DM (State (WorklistState lattice) a)
  deriving (Functor, Applicative, Monad)

type ChangeDetector lattice
  = Arrows (Domains lattice) (CoDomain lattice -> CoDomain lattice -> Bool)

-- | Goal:
-- @
--   TransferFunction m Int = m Int
--   TransferFunction m (Bool -> Int) = Bool -> m Int
--   TransferFunction m (a -> b -> Int) = a -> b -> m Int
--   TransferFunction m (a -> b -> c -> Int) = a -> b -> c -> m Int
-- @
type TransferFunction m lattice
  = Arrows (Domains lattice) (m (CoDomain lattice))

type Datafixable lattice =
  ( Currying (Domains lattice) (CoDomain lattice)
  , Currying (Domains lattice) (DependencyM lattice (CoDomain lattice))
  , Currying (Domains lattice) (CoDomain lattice -> CoDomain lattice -> Bool)
  , MonoMapKey (Products (Domains lattice))
  , BoundedJoinSemiLattice (CoDomain lattice)
  )

data DataFlowProblem lattice
  = DFP
  { transfer     :: GraphNode -> TransferFunction (DependencyM lattice) lattice
  , detectChange :: GraphNode -> ChangeDetector lattice
  }

eqChangeDetector
  :: forall lattice
   . Eq (CoDomain lattice)
  => Datafixable lattice
  => Proxy lattice -> ChangeDetector lattice
eqChangeDetector _ =
  currys (Proxy :: Proxy (Domains lattice)) (Proxy :: Proxy (CoDomain lattice -> CoDomain lattice -> Bool)) $
    const (/=)

alwaysChangeDetector
  :: forall lattice
   . Datafixable lattice
  => Proxy lattice -> ChangeDetector lattice
alwaysChangeDetector _ =
  currys (Proxy :: Proxy (Domains lattice)) (Proxy :: Proxy (CoDomain lattice -> CoDomain lattice -> Bool)) $
    \_ _ _ -> True

data NodeInfo lattice
  = NodeInfo
  { value      :: !(Maybe (CoDomain lattice)) -- ^ the value at this node. Can be Nothing only when a loop was detected
  , references :: !(IntArgsMonoSet (Products (Domains lattice)))      -- ^ nodes this value depends on
  , referrers  :: !(IntArgsMonoSet (Products (Domains lattice)))      -- ^ nodes depending on this value
  }

deriving instance (Eq (CoDomain lattice), Eq (IntArgsMonoSet (Products (Domains lattice)))) => Eq (NodeInfo lattice)
deriving instance (Show (CoDomain lattice), Show (IntArgsMonoSet (Products (Domains lattice)))) => Show (NodeInfo lattice)

emptyNodeInfo :: NodeInfo lattice
emptyNodeInfo = NodeInfo Nothing IntArgsMonoSet.empty IntArgsMonoSet.empty

type Graph lattice
  = IntArgsMonoMap (Products (Domains lattice)) (NodeInfo lattice)

data WorklistState lattice
  = WorklistState
  { problem         :: !(DataFlowProblem lattice)
  , graph           :: !(Graph lattice)
  , unstable        :: !(IntArgsMonoSet (Products (Domains lattice))) -- unstable nodes and their changed references
  , callStack       :: !(IntArgsMonoSet (Products (Domains lattice)))
  , referencedNodes :: !(IntArgsMonoSet (Products (Domains lattice)))
  }

zoomGraph :: State (Graph lattice) a -> State (WorklistState lattice) a
zoomGraph modifyGraph = state $ \st ->
  let (res, g) = runState modifyGraph (graph st)
  in  (res, st { graph = g })

zoomUnstable
  :: State (IntArgsMonoSet (Products (Domains lattice))) a
  -> State (WorklistState lattice) a
zoomUnstable modifyUnstable = state $ \st ->
  let (res, u) = runState modifyUnstable (unstable st)
  in  (res, st { unstable = u })

zoomReferencedNodes
  :: State (IntArgsMonoSet (Products (Domains lattice))) a
  -> State (WorklistState lattice) a
zoomReferencedNodes modifier = state $ \st ->
  let (res, rn) = runState modifier (referencedNodes st)
  in  (res, st { referencedNodes = rn })

initialWorklistState
  :: IntArgsMonoSet (Products (Domains lattice))
  -> DataFlowProblem lattice
  -> WorklistState lattice
initialWorklistState unstable_ fw =
  WorklistState fw IntArgsMonoMap.empty unstable_ IntArgsMonoSet.empty IntArgsMonoSet.empty

dependOn
  :: forall lattice
   . Datafixable lattice
  => Proxy lattice -> GraphNode -> TransferFunction (DependencyM lattice) lattice
dependOn _ (GraphNode node) = currys dom cod impl
  where
    dom = Proxy :: Proxy (Domains lattice)
    cod = Proxy :: Proxy (DependencyM lattice (CoDomain lattice))
    impl args = DM $ do
      loopDetected <- IntArgsMonoSet.member node args <$> gets callStack
      -- isNotYetStable <- Map.member node <$> gets unstable
      maybeNodeInfo <- IntArgsMonoMap.lookup node args <$> gets graph
      -- save that we depend on this value
      zoomReferencedNodes (modify' (IntArgsMonoSet.insert node args))
      case maybeNodeInfo of
        Nothing | loopDetected ->
          -- Somewhere in an outer call stack we already compute this one.
          -- We don't recurse again and just return 'bottom'.
          -- The outer call will then recognize the instability and enqueue
          -- itself as unstable after its first approximation is computed.
          return bottom
        Nothing ->
          -- Depth-first discovery of reachable nodes.
          recompute node args
        -- We aren't doing this because it's not obvious
        -- which nodes will need to be marked unstable.
        -- Think about a node that `dependOn`s the same
        -- node twice.
        -- Also it is unclear if this really is beneficial:
        -- We don't discover any new nodes and should rather
        -- rely on the ordering in the worklist.
        --Just _ | isNotYetStable && not loopDetected -> do
        --  recompute node
        Just info -> return (fromMaybe bottom (value info))
{-# INLINE dependOn #-}

unsafePeekValue
  :: forall lattice
   . MonoMapKey (Products (Domains lattice))
  => Currying (Domains lattice) (DependencyM lattice (Maybe (CoDomain lattice)))
  => GraphNode
  -> Arrows (Domains lattice) (DependencyM lattice (Maybe (CoDomain lattice)))
unsafePeekValue (GraphNode node) =
  currys (Proxy :: Proxy (Domains lattice)) (Proxy :: Proxy (DependencyM lattice (Maybe (CoDomain lattice)))) $ \args ->
    DM $ (value <=< IntArgsMonoMap.lookup node args) <$> gets graph

data Diff a
  = Diff
  { added   :: !(IntArgsMonoSet a)
  , removed :: !(IntArgsMonoSet a)
  }

computeDiff
  :: k ~ Products (Domains lattice)
  => MonoMapKey k
  => Proxy lattice 
  -> IntArgsMonoSet k
  -> IntArgsMonoSet k
  -> Diff k
computeDiff _ from to = Diff (to `IntArgsMonoSet.difference` from) (from `IntArgsMonoSet.difference` to)

updateGraphNode
  :: forall lattice
   . MonoMapKey (Products (Domains lattice))
  => Int
  -> Products (Domains lattice)
  -> CoDomain lattice
  -> IntArgsMonoSet (Products (Domains lattice))
  -> State (WorklistState lattice) (NodeInfo lattice)
updateGraphNode node args val refs = zoomGraph $ do
  -- if we are lucky (e.g. no refs changed), we get away with one map access
  -- first update `node`s NodeInfo
  let newInfo = emptyNodeInfo { value = Just val, references = refs }
  let merger _ _ new old = new { referrers = referrers old }
  oldInfo <- fromMaybe emptyNodeInfo <$>
    state (IntArgsMonoMap.insertLookupWithKey merger node args newInfo)

  -- Now compute the diff of changed references
  let diff = computeDiff (Proxy :: Proxy lattice) (references oldInfo) refs

  -- finally register/unregister at all references as referrer.
  let updater f (depNode, depArgs) = modify' $
        IntArgsMonoMap.alter (Just . f . fromMaybe emptyNodeInfo) depNode depArgs
  let addReferrer ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
  let removeReferrer ni = ni { referrers = IntArgsMonoSet.delete node args (referrers ni) }
  forM_ (IntArgsMonoSet.toList (added diff)) (updater addReferrer)
  forM_ (IntArgsMonoSet.toList (removed diff)) (updater removeReferrer)

  return oldInfo
{-# INLINE updateGraphNode #-}

recompute
  :: forall lattice dom cod
   . dom ~ Domains lattice
  => cod ~ CoDomain lattice
  => Datafixable lattice
  => Int -> Products dom -> State (WorklistState lattice) cod
recompute node args = do
  oldState <- get
  put $ oldState
    { referencedNodes = IntArgsMonoSet.empty
    , callStack = IntArgsMonoSet.insert node args (callStack oldState)
    }
  let dom = Proxy :: Proxy dom
  let depm = Proxy :: Proxy (DependencyM lattice cod)
  let eq = Proxy :: Proxy (cod -> cod -> Bool)
  let node' = GraphNode node
  let DM iterate' = uncurrys dom depm (transfer (problem oldState) node') args
  let detectChange' = uncurrys dom eq (detectChange (problem oldState) node') args
  newVal <- iterate'
  refs <- gets referencedNodes
  oldInfo <- updateGraphNode node args newVal refs
  deleteUnstable node args
  case value oldInfo of
    Just oldVal | not (detectChange' oldVal newVal) -> return ()
    _ -> do
      forM_ (IntArgsMonoSet.toList (referrers oldInfo)) (uncurry enqueueUnstable)
      -- If the node depends on itself the first time (e.g. when it gets its first value),
      -- that will not be reflected in `oldInfo`. So we enqueue it manually
      -- if that is the case.
      when (IntArgsMonoSet.member node args refs) (enqueueUnstable node args)
  modify' $ \st -> st
    { referencedNodes = referencedNodes oldState
    , callStack = callStack oldState
    }
  return newVal
{-# INLINE recompute #-}

enqueueUnstable
  :: k ~ Products (Domains lattice)
  => MonoMapKey k
  => Int -> k -> State (WorklistState lattice) ()
enqueueUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.insert i k))

deleteUnstable
  :: k ~ Products (Domains lattice)
  => MonoMapKey k
  => Int -> k -> State (WorklistState lattice) ()
deleteUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.delete i k))

highestPriorityUnstableNode
  :: k ~ Products (Domains lattice)
  => MonoMapKey k
  => State (WorklistState lattice) (Maybe (Int, k))
highestPriorityUnstableNode = listToMaybe . IntArgsMonoSet.highestPriorityNodes <$> gets unstable

whileJust_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ cond action = go
  where
    go = cond >>= \m -> case m of
      Nothing -> return ()
      Just a  -> action a >> go
{-# INLINE whileJust_ #-}

work :: Datafixable lattice => State (WorklistState lattice) ()
work = whileJust_ highestPriorityUnstableNode (uncurry recompute)
{-# INLINE work #-}

fixProblem
  :: forall lattice dom cod
   . dom ~ Domains lattice
  => cod ~ CoDomain lattice
  => Datafixable lattice
  => DataFlowProblem lattice
  -> GraphNode
  -> Arrows dom cod
fixProblem prob (GraphNode node) = currys (Proxy :: Proxy dom) (Proxy :: Proxy cod) impl
  where
    impl args
      = fromMaybe (error "Broken invariant: The root node has no value")
      . (>>= value)
      . IntArgsMonoMap.lookup node args
      . graph
      . execState work
      . initialWorklistState (IntArgsMonoSet.singleton node args)
      $ prob
{-# INLINE fixProblem #-}
