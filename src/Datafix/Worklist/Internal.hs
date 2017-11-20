{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Datafix.Worklist.Internal where

import           Algebra.Lattice
import           Control.Monad                    (forM_, guard, when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.IORef
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import           Data.Proxy                       (Proxy (..))
import           Datafix                          hiding (dependOn)
import qualified Datafix
import           Datafix.IntArgsMonoSet           (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet           as IntArgsMonoSet
import           Datafix.MonoMap                  (MonoMapKey)
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph           (GraphRef, NodeInfo (..))
import qualified Datafix.Worklist.Graph           as Graph
import qualified Datafix.Worklist.Graph.Dense     as DenseGraph
import qualified Datafix.Worklist.Graph.Sparse    as SparseGraph
import           System.IO.Unsafe                 (unsafePerformIO)

newtype DependencyM graph domain a
  = DM (ReaderT (Env graph domain) IO a)
  -- ^ Why does this use 'IO'? Actually, we only need 'IO' here, but that
  -- means we have to carry around the state thread in type signatures.
  --
  -- This ultimately leaks badly into the exported interface in 'fixProblem':
  -- Since we can't have universally quantified instance contexts (yet!), we can' write
  -- @(forall s. Datafixable (DependencyM s graph domain)) => (forall s. DataFlowProblem (DependencyM s graph domain)) -> ...@
  -- and have to instead have the isomorphic
  -- @(forall s r. (Datafixable (DependencyM s graph domain) => r) -> r) -> (forall s. DataFlowProblem (DependencyM s graph domain)) -> ...@
  -- and urge all call sites to pass a meaningless 'id' parameter.
  --
  -- Also, this means more explicit type signatures as we have to make clear to
  -- the type-checker that @s@ is universally quantified in everything that
  -- touches it, e.g. @Analyses.StrAnal.LetDn.buildProblem@ from the test suite.
  --
  -- So, bottom line: We resort to 'IO' and 'unsafePerformIO' and promise not to
  -- launch missiles. In particular, we don't export 'DM' and also there
  -- must never be an instance of 'MonadIO' for this.
  deriving (Functor, Applicative, Monad)

data Env graph domain
  = Env
  { problem          :: !(DataFlowProblem (DependencyM graph domain))
  -- ^ Constant.
  -- The specification of the data-flow problem we ought to solve.
  , iterationBound   :: !(IterationBound domain)
  -- ^ Constant.
  -- Whether to abort after a number of iterations or not.
  , callStack        :: !(IntArgsMonoSet (Products (Domains domain)))
  -- ^ Contextual state.
  -- The set of points in the 'domain' of 'Node's currently in the call stack.
  , graph            :: !(graph domain)
  -- ^ Constant ref to stateful graph.
  -- The data-flow graph, modeling dependencies between data-flow 'Node's,
  -- or rather specific points in the 'domain' of each 'Node'.
  , referencedPoints :: !(IORef (IntArgsMonoSet (Products (Domains domain))))
  -- ^ Constant (but the the wrapped queue is stateful).
  -- The set of points the currently 'recompute'd node references so far.
  , unstable         :: !(IORef (IntArgsMonoSet (Products (Domains domain))))
  -- ^ Constant (but the the wrapped queue is stateful).
  -- Unstable nodes that will be 'recompute'd by the 'work'list algorithm.
  }

initialEnv
  :: GraphRef graph
  => IntArgsMonoSet (Products (Domains domain))
  -> DataFlowProblem (DependencyM graph domain)
  -> IterationBound domain
  -> IO (graph domain)
  -> IO (Env graph domain)
initialEnv unstable_ prob ib newGraphRef =
  Env prob ib IntArgsMonoSet.empty
    <$> newGraphRef
    <*> newIORef IntArgsMonoSet.empty
    <*> newIORef unstable_
{-# INLINE initialEnv #-}

type Datafixable m =
  ( Currying (Domains (Domain m)) (CoDomain (Domain m))
  , Currying (Domains (Domain m)) (m (CoDomain (Domain m)))
  , Currying (Domains (Domain m)) (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)
  , Currying (Domains (Domain m)) (CoDomain (Domain m) -> CoDomain (Domain m))
  , MonoMapKey (Products (Domains (Domain m)))
  , BoundedJoinSemiLattice (CoDomain (Domain m))
  )

instance (Datafixable (DependencyM graph domain), GraphRef graph) => MonadDependency (DependencyM graph domain) where
  type Domain (DependencyM graph domain) = domain
  dependOn = dependOn
  {-# INLINE dependOn #-}

data Density graph where
  Sparse :: Density SparseGraph.Ref
  Dense :: Node -> Density DenseGraph.Ref

type AbortionFunction domain
  = Arrows (Domains domain) (CoDomain domain -> CoDomain domain)

data IterationBound domain
  = NeverAbort
  | AbortAfter Int (AbortionFunction domain)

zoomIORef
  :: State s a
  -> ReaderT (IORef s) IO a
zoomIORef s = do
  ref <- ask
  uns <- lift $ readIORef ref
  let (res, uns') = runState s uns
  uns' `seq` lift $ writeIORef ref uns'
  return res
{-# INLINE zoomIORef #-}

zoomReferencedPoints
  :: State (IntArgsMonoSet (Products (Domains domain))) a
  -> ReaderT (Env graph domain) IO a
zoomReferencedPoints = withReaderT referencedPoints . zoomIORef
{-# INLINE zoomReferencedPoints #-}

zoomUnstable
  :: State (IntArgsMonoSet (Products (Domains domain))) a
  -> ReaderT (Env graph domain) IO a
zoomUnstable = withReaderT unstable . zoomIORef
{-# INLINE zoomUnstable #-}

enqueueUnstable
  :: k ~ Products (Domains domain)
  => MonoMapKey k
  => Int -> k -> ReaderT (Env graph domain) IO ()
enqueueUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.insert i k))
{-# INLINE enqueueUnstable #-}

deleteUnstable
  :: k ~ Products (Domains domain)
  => MonoMapKey k
  => Int -> k -> ReaderT (Env graph domain) IO ()
deleteUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.delete i k))
{-# INLINE deleteUnstable #-}

highestPriorityUnstableNode
  :: k ~ Products (Domains domain)
  => MonoMapKey k
  => ReaderT (Env graph domain) IO (Maybe (Int, k))
highestPriorityUnstableNode = zoomUnstable $
  listToMaybe . IntArgsMonoSet.highestPriorityNodes <$> get
{-# INLINE highestPriorityUnstableNode #-}

withCall
  :: Datafixable (DependencyM graph domain)
  => Int
  -> Products (Domains domain)
  -> ReaderT (Env graph domain) IO a
  -> ReaderT (Env graph domain) IO a
withCall node args r = ReaderT $ \env -> do
  refs <- readIORef (referencedPoints env)
  refs `seq` writeIORef (referencedPoints env) IntArgsMonoSet.empty
  ret <- runReaderT r env
    { callStack = IntArgsMonoSet.insert node args (callStack env)
    }
  writeIORef (referencedPoints env) refs
  return ret
{-# INLINE withCall #-}

recompute
  :: forall domain graph dom cod
   . dom ~ Domains domain
  => cod ~ CoDomain domain
  => GraphRef graph
  => Datafixable (DependencyM graph domain)
  => Int -> Products dom -> ReaderT (Env graph domain) IO cod
recompute node args = withCall node args $ do
  prob <- asks problem
  let dom = Proxy :: Proxy dom
  let depm = Proxy :: Proxy (DependencyM graph domain cod)
  let eq = Proxy :: Proxy (cod -> cod -> Bool)
  let cod2cod = Proxy :: Proxy (cod -> cod)
  let node' = Node node
  let DM iterate' = uncurrys dom depm (dfpTransfer prob node') args
  let detectChange' = uncurrys dom eq (dfpDetectChange prob node') args
  -- We need to access the graph at three different points in time:
  --
  --    1. before the call to 'iterate', to access 'iterations', but only if abortion is required
  --    2. directly after the call to 'iterate', to get the 'oldInfo'
  --    3. And again to actually write the 'newInfo'
  --
  -- The last two can be merged, whereas it's crucial that 'oldInfo'
  -- is captured *after* the call to 'iterate', otherwise we might
  -- not pick up all 'referrers'.
  -- If abortion is required, 'maybeAbortedVal' will not be 'Nothing'.
  maybeAbortedVal <- runMaybeT $ do
    AbortAfter n abort <- lift (asks iterationBound)
    Just preInfo <- lift (withReaderT graph (Graph.lookup node args))
    guard (iterations preInfo >= n)
    Just oldVal <- return (value preInfo)
    return (uncurrys dom cod2cod abort args oldVal)
  -- For the 'Nothing' case, we proceed by iterating the transfer function.
  newVal <- maybe iterate' return maybeAbortedVal
  -- When abortion is required, 'iterate'' is not called and
  -- 'refs' will be empty, thus the node will never be marked unstable again.
  refs <- asks referencedPoints >>= lift . readIORef
  oldInfo <- withReaderT graph (Graph.updatePoint node args newVal refs)
  deleteUnstable node args
  case value oldInfo of
    Just oldVal | not (detectChange' oldVal newVal) ->
      return ()
    _ -> do
      forM_ (IntArgsMonoSet.toList (referrers oldInfo)) $
        uncurry enqueueUnstable
      when (IntArgsMonoSet.member node args refs) $
        -- This is a little unfortunate: The 'oldInfo' will
        -- not have listed the current node itself as a refererrer
        -- in case of a loop, so we have to check for
        -- that case manually in the new 'references' set.
        -- The info stored in the graph has the right 'referrers'
        -- set, though.
        enqueueUnstable node args
  return newVal
{-# INLINE recompute #-}

dependOn
  :: forall domain graph
   . Datafixable (DependencyM graph domain)
  => GraphRef graph
  => Proxy (DependencyM graph domain) -> Node -> TransferFunction (DependencyM graph domain) domain
dependOn _ (Node node) = currys dom cod impl
  where
    dom = Proxy :: Proxy (Domains domain)
    cod = Proxy :: Proxy (DependencyM graph domain (CoDomain domain))
    impl args = DM $ do
      cycleDetected <- IntArgsMonoSet.member node args <$> asks callStack
      isStable <- zoomUnstable $
        not . IntArgsMonoSet.member node args <$> get
      maybeNodeInfo <- withReaderT graph (Graph.lookup node args)
      zoomReferencedPoints (modify' (IntArgsMonoSet.insert node args))
      case maybeNodeInfo >>= value of
        -- 'value' can only be 'Nothing' if there was a 'cycleDetected':
        -- Otherwise, the node wasn't part of the call stack and thus will either
        -- have a 'value' assigned or will not have been discovered at all.
        Nothing | cycleDetected ->
          -- Somewhere in an outer activation record we already compute this one.
          -- We don't recurse again and just return an optimistic approximation,
          -- such as 'bottom'.
          -- Otherwise, 'recompute' will immediately add a 'NodeInfo' before
          -- any calls to 'dependOn' for a cycle to even be possible.
          optimisticApproximation node args
        Just val | isStable || cycleDetected ->
          -- No brainer
          return val
        maybeVal ->
          -- No cycle && (unstable || undiscovered). Apply one of the schemes
          -- outlined in
          -- https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like
          scheme2 maybeVal node args
{-# INLINE dependOn #-}

-- | Compute an optimistic approximation for a point of a given node that is
-- as precise as possible, given the other points of that node we already
-- computed.
--
-- E.g., it is always valid to return 'bottom' from this, but in many cases
-- we can be more precise since we possibly have computed points for the node
-- that are lower bounds to the current point.
optimisticApproximation
  :: GraphRef graph
  => Datafixable (DependencyM graph domain)
  => Int -> Products (Domains domain) -> ReaderT (Env graph domain) IO (CoDomain domain)
optimisticApproximation node args = do
  points <- withReaderT graph (Graph.lookupLT node args)
  -- Note that 'points' might contain 'NodeInfo's that have no 'value'.
  -- It's OK to filter these out: At worst, the approximation will be
  -- more optimistic than necessary.
  return (joins (mapMaybe (value . snd) points))

scheme1, scheme2
  :: GraphRef graph
  => Datafixable (DependencyM graph domain)
  => Maybe (CoDomain domain)
  -> Int
  -> Products (Domains domain)
  -> ReaderT (Env graph domain) IO (CoDomain domain)
{-# INLINE scheme1 #-}
{-# INLINE scheme2 #-}

-- | scheme 1 (see https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like).
--
-- Let the worklist algorithm figure things out.
scheme1 maybeVal node args =
  case maybeVal of
    Nothing -> do
      enqueueUnstable node args
      optimisticApproximation node args
    Just val ->
      return val

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

whileJust_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ cond action = go
  where
    go = cond >>= \case
      Nothing -> return ()
      Just a  -> action a >> go
{-# INLINE whileJust_ #-}

work
  :: GraphRef graph
  => Datafixable (DependencyM graph domain)
  => ReaderT (Env graph domain) IO ()
work = whileJust_ highestPriorityUnstableNode (uncurry recompute)
{-# INLINE work #-}

fixProblem
  :: forall domain graph
   . GraphRef graph
  => Datafixable (DependencyM graph domain)
  => DataFlowProblem (DependencyM graph domain)
  -> Density graph
  -> IterationBound domain
  -> Node
  -> Arrows (Domains domain) (CoDomain domain)
fixProblem prob density ib (Node node) = currys (Proxy :: Proxy (Domains domain)) (Proxy :: Proxy (CoDomain domain)) impl
  where
    impl
      = fromMaybe (error "Broken invariant: The root node has no value")
      . (>>= value)
      . runProblem
    runProblem args = unsafePerformIO $ do
      -- Trust me, I'm an engineer! See the docs of the 'DM' constructor
      -- of 'DependencyM' for why we 'unsafePerformIO'.
      let newGraphRef = case density of
            Sparse               -> SparseGraph.newRef
            Dense (Node maxNode) -> DenseGraph.newRef (maxNode + 1)
      env <- initialEnv (IntArgsMonoSet.singleton node args) prob ib newGraphRef
      runReaderT (work >> withReaderT graph (Graph.lookup node args)) env
{-# INLINE fixProblem #-}
