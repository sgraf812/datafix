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
import           Control.Monad                    (forM_, (<=<))
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
{-# INLINE eqChangeDetector #-}

alwaysChangeDetector
  :: forall lattice
   . Datafixable lattice
  => Proxy lattice -> ChangeDetector lattice
alwaysChangeDetector _ =
  currys (Proxy :: Proxy (Domains lattice)) (Proxy :: Proxy (CoDomain lattice -> CoDomain lattice -> Bool)) $
    \_ _ _ -> True
{-# INLINE alwaysChangeDetector #-}

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
{-# INLINE emptyNodeInfo #-}

type Graph lattice
  = IntArgsMonoMap (Products (Domains lattice)) (NodeInfo lattice)

data WorklistState lattice
  = WorklistState
  { problem         :: !(DataFlowProblem lattice)
  , graph           :: !(Graph lattice)
  , unstable        :: !(IntArgsMonoSet (Products (Domains lattice))) -- unstable nodes and their changed references
  , callStack       :: !(IntArgsMonoSet (Products (Domains lattice)))
  , current         :: !(Maybe (Int, Products (Domains lattice)))
  }

initialWorklistState
  :: IntArgsMonoSet (Products (Domains lattice))
  -> DataFlowProblem lattice
  -> WorklistState lattice
initialWorklistState unstable_ fw =
  WorklistState fw IntArgsMonoMap.empty unstable_ IntArgsMonoSet.empty Nothing
{-# INLINE initialWorklistState #-}

zoomGraph :: State (Graph lattice) a -> State (WorklistState lattice) a
zoomGraph modifyGraph = state $ \st ->
  let (res, g) = runState modifyGraph (graph st)
  in  (res, st { graph = g })
{-# INLINE zoomGraph #-}

zoomUnstable
  :: State (IntArgsMonoSet (Products (Domains lattice))) a
  -> State (WorklistState lattice) a
zoomUnstable modifyUnstable = state $ \st ->
  let (res, u) = runState modifyUnstable (unstable st)
  in  (res, st { unstable = u })
{-# INLINE zoomUnstable #-}

enqueueUnstable
  :: k ~ Products (Domains lattice)
  => MonoMapKey k
  => Int -> k -> State (WorklistState lattice) ()
enqueueUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.insert i k))
{-# INLINE enqueueUnstable #-}

deleteUnstable
  :: k ~ Products (Domains lattice)
  => MonoMapKey k
  => Int -> k -> State (WorklistState lattice) ()
deleteUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.delete i k))
{-# INLINE deleteUnstable #-}

highestPriorityUnstableNode
  :: k ~ Products (Domains lattice)
  => MonoMapKey k
  => State (WorklistState lattice) (Maybe (Int, k))
highestPriorityUnstableNode = listToMaybe . IntArgsMonoSet.highestPriorityNodes <$> gets unstable
{-# INLINE highestPriorityUnstableNode #-}

clearReferences 
  :: forall lattice
   . MonoMapKey (Products (Domains lattice))
  => Int
  -> Products (Domains lattice)
  -> State (WorklistState lattice) (NodeInfo lattice)
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
  :: forall lattice
   . MonoMapKey (Products (Domains lattice))
  => Datafixable lattice
  => Int
  -> Products (Domains lattice)
  -> CoDomain lattice
  -> State (WorklistState lattice) (NodeInfo lattice)
updateNodeValue node args val = zoomGraph $ do
  let updater _ _ ni = Just ni { value = Just val }
  oldInfo <- fromMaybe (error "There should be an entry when this is called") <$>
    state (IntArgsMonoMap.updateLookupWithKey updater node args)
  return oldInfo { value = Just val }
{-# INLINE updateNodeValue #-}

withCall 
  :: Datafixable lattice 
  => Int 
  -> Products (Domains lattice) 
  -> State (WorklistState lattice) a 
  -> State (WorklistState lattice) a
withCall node args inner = do
  old <- get
  put $ old
    { callStack = IntArgsMonoSet.insert node args (callStack old)
    , current = Just (node, args)
    }
  ret <- inner
  modify' $ \new -> new
    { callStack = callStack old
    , current = current old
    }
  return ret
{-# INLINE withCall #-}

recompute
  :: forall lattice dom cod
   . dom ~ Domains lattice
  => cod ~ CoDomain lattice
  => Datafixable lattice
  => Int -> Products dom -> State (WorklistState lattice) cod
recompute node args = withCall node args $ do
  deleteUnstable node args
  maybeOldVal <- value <$> clearReferences node args
  prob <- gets problem
  let dom = Proxy :: Proxy dom
  let depm = Proxy :: Proxy (DependencyM lattice cod)
  let eq = Proxy :: Proxy (cod -> cod -> Bool)
  let node' = GraphNode node
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
  :: forall lattice
   . MonoMapKey (Products (Domains lattice))
  => Int
  -> Products (Domains lattice)
  -> Int
  -> Products (Domains lattice)
  -> State (WorklistState lattice) ()
addReference node args depNode depArgs = zoomGraph $ do
  let adjustReferences ni = ni { references = IntArgsMonoSet.insert depNode depArgs (references ni) }
  modify' (IntArgsMonoMap.adjust adjustReferences node args)
  let adjustReferrers ni = ni { referrers = IntArgsMonoSet.insert node args (referrers ni) }
  modify' (IntArgsMonoMap.adjust adjustReferrers depNode depArgs)
{-# INLINE addReference #-}

dependOn
  :: forall lattice
   . Datafixable lattice
  => Proxy lattice -> GraphNode -> TransferFunction (DependencyM lattice) lattice
dependOn _ (GraphNode node) = currys dom cod impl
  where
    dom = Proxy :: Proxy (Domains lattice)
    cod = Proxy :: Proxy (DependencyM lattice (CoDomain lattice))
    impl args = DM $ do
      cycleDetected <- IntArgsMonoSet.member node args <$> gets callStack
      isStable <- not . IntArgsMonoSet.member node args <$> gets unstable
      maybeNodeInfo <- IntArgsMonoMap.lookup node args <$> gets graph
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
      (curNode, curArgs) <- fromMaybe (error "`dependOn` can only be called in an activation record") <$> gets current
      addReference curNode curArgs node args 
      return val
{-# INLINE dependOn #-}

scheme1, scheme2, scheme3
  :: Datafixable lattice
  => Maybe (CoDomain lattice)
  -> Int
  -> Products (Domains lattice) 
  -> State (WorklistState lattice) (CoDomain lattice)
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

unsafePeekValue
  :: forall lattice
   . MonoMapKey (Products (Domains lattice))
  => Currying (Domains lattice) (DependencyM lattice (Maybe (CoDomain lattice)))
  => GraphNode
  -> Arrows (Domains lattice) (DependencyM lattice (Maybe (CoDomain lattice)))
unsafePeekValue (GraphNode node) =
  currys (Proxy :: Proxy (Domains lattice)) (Proxy :: Proxy (DependencyM lattice (Maybe (CoDomain lattice)))) $ \args ->
    DM $ (value <=< IntArgsMonoMap.lookup node args) <$> gets graph
{-# INLINE unsafePeekValue #-}

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
