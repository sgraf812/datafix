{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Datafix.Worklist.Internal where

import           Algebra.Lattice
import           Control.Monad                 (forM_)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Maybe                    (fromMaybe, listToMaybe,
                                                mapMaybe)
import           Data.Proxy                    (Proxy (..))
import           Datafix                       hiding (dependOn)
import qualified Datafix
import           Datafix.IntArgsMonoSet        (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet        as IntArgsMonoSet
import           Datafix.MonoMap               (MonoMapKey)
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph        (GraphRef, NodeInfo (..))
import qualified Datafix.Worklist.Graph        as Graph
import qualified Datafix.Worklist.Graph.Dense  as DenseGraph
import qualified Datafix.Worklist.Graph.Sparse as SparseGraph
import           System.IO.Unsafe              (unsafePerformIO)

newtype DependencyM graph domain a
  = DM (ReaderT (Env graph domain) IO a)
  -- ^ Why does this use 'IO'? Actually, we only need `IO` here, but that
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
  { problem        :: !(DataFlowProblem (DependencyM graph domain))
  -- ^ Constant.
  -- The specification of the data-flow problem we ought to solve.
  , iterationBound :: !(IterationBound domain)
  -- ^ Constant.
  -- Whether to abort after a number of iterations or not.
  , callStack      :: !(IntArgsMonoSet (Products (Domains domain)))
  -- ^ Contextual state.
  -- The set of points in the 'domain' of 'Node's currently in the call stack.
  , current        :: !(Maybe (Int, Products (Domains domain)))
  -- ^ Contextual state.
  -- The specific point in the 'domain' of a 'Node' we currently 'recompute'.
  , graph          :: !(graph domain)
  -- ^ Constant ref to stateful graph.
  -- The data-flow graph, modeling dependencies between data-flow 'Node's,
  -- or rather specific points in the 'domain' of each 'Node'.
  , unstable       :: !(IORef (IntArgsMonoSet (Products (Domains domain))))
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
  Env prob ib IntArgsMonoSet.empty Nothing
    <$> newGraphRef
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

zoomUnstable
  :: State (IntArgsMonoSet (Products (Domains domain))) a
  -> ReaderT (Env graph domain) IO a
zoomUnstable modifyUnstable = do
  ref <- asks unstable
  uns <- lift $ readIORef ref
  let (res, uns') = runState modifyUnstable uns
  lift $ writeIORef ref uns'
  return res
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
withCall node args = local $ \env -> env
  { callStack = IntArgsMonoSet.insert node args (callStack env)
  , current = Just (node, args)
  }
{-# INLINE withCall #-}

recompute
  :: forall domain graph dom cod
   . dom ~ Domains domain
  => cod ~ CoDomain domain
  => GraphRef graph
  => Datafixable (DependencyM graph domain)
  => Int -> Products dom -> ReaderT (Env graph domain) IO cod
recompute node args = withCall node args $ do
  deleteUnstable node args
  oldInfo <- withReaderT graph (Graph.clearReferences node args)
  prob <- asks problem
  let dom = Proxy :: Proxy dom
  let depm = Proxy :: Proxy (DependencyM graph domain cod)
  let eq = Proxy :: Proxy (cod -> cod -> Bool)
  let cod2cod = Proxy :: Proxy (cod -> cod)
  let node' = Node node
  let DM iterate' = uncurrys dom depm (transfer prob node') args
  let detectChange' = uncurrys dom eq (detectChange prob node') args
  ib <- asks iterationBound
  newVal <- case (value oldInfo, ib) of
    (Just oldVal, AbortAfter n abort)
      | iterations oldInfo >= n
      -> return (uncurrys dom cod2cod abort args oldVal)
    _ -> iterate'
  newInfo <- withReaderT graph (Graph.updateNodeValue node args newVal)
  case value oldInfo of
    Just oldVal | not (detectChange' oldVal newVal) ->
      return ()
    _ ->
      forM_ (IntArgsMonoSet.toList (referrers newInfo)) (uncurry enqueueUnstable)
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
      val <-
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
      -- save that we depend on this value
      (curNode, curArgs) <- fromMaybe (error "`dependOn` can only be called in an activation record") <$> asks current
      withReaderT graph (Graph.addReference curNode curArgs node args)
      return val
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

scheme1, scheme2, scheme3
  :: GraphRef graph
  => Datafixable (DependencyM graph domain)
  => Maybe (CoDomain domain)
  -> Int
  -> Products (Domains domain)
  -> ReaderT (Env graph domain) IO (CoDomain domain)
{-# INLINE scheme1 #-}
{-# INLINE scheme2 #-}
{-# INLINE scheme3 #-}

-- | scheme 1 (see https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like).
--
-- Let the worklist algorithm figure things out.
scheme1 _ = optimisticApproximation

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
