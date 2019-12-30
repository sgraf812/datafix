{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      :  Datafix.Worklist.Internal
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Internal module, does not follow the PVP. Breaking changes may happen at
-- any minor version.

module Datafix.Worklist.Internal where

import           Algebra.Lattice
import           Control.Monad                    (forM_, guard, when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.IORef
import           Data.Maybe                       (listToMaybe, mapMaybe)
import           Datafix.Common
import           Datafix.Entailments
import           Datafix.Explicit                 hiding (dependOn)
import qualified Datafix.Explicit
import           Datafix.IntArgsMonoSet           (IntArgsMonoSet)
import qualified Datafix.IntArgsMonoSet           as IntArgsMonoSet
import           Datafix.MonoMap                  (MonoMapKey)
import           Datafix.Utils.Constraints
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist.Graph           (GraphRef, PointInfo (..))
import qualified Datafix.Worklist.Graph           as Graph
import qualified Datafix.Worklist.Graph.Dense     as DenseGraph
import qualified Datafix.Worklist.Graph.Sparse    as SparseGraph
import           System.IO.Unsafe                 (unsafePerformIO)

-- | The concrete 'MonadDependency' for this worklist-based solver.
--
-- This essentially tracks the current approximation of the solution to the
-- 'DataFlowProblem' as mutable state while 'solveProblem' makes sure we will eventually
-- halt with a conservative approximation.
newtype DependencyM graph domain a
  = DM (ReaderT (Env graph domain) IO a)
  -- ^ Why does this use 'IO'? Actually, we only need 'ST' here, but that
  -- means we have to carry around the state thread in type signatures.
  --
  -- This ultimately leaks badly into the exported interface in 'solveProblem':
  -- Since we can't have universally quantified instance contexts (yet!), we can' write
  -- @(forall s. Datafixable domain => (forall s. DataFlowProblem (DependencyM s graph domain)) -> ...@
  -- and have to instead have the isomorphic
  -- @(forall s r. (Datafixable domain => r) -> r) -> (forall s. DataFlowProblem (DependencyM s graph domain)) -> ...@
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

-- | The iteration state of 'DependencyM'/'solveProblem'.
data Env graph domain
  = Env
  { problem          :: !(DataFlowProblem (DependencyM graph domain))
  -- ^ Constant.
  -- The specification of the data-flow problem we ought to solve.
  , iterationBound   :: !(IterationBound domain)
  -- ^ Constant.
  -- Whether to abort after a number of iterations or not.
  , callStack        :: !(IntArgsMonoSet (Products (ParamTypes domain)))
  -- ^ Contextual state.
  -- The set of points in the 'domain' of 'Node's currently in the call stack.
  , graph            :: !(graph domain)
  -- ^ Constant ref to stateful graph.
  -- The data-flow graph, modeling dependencies between data-flow 'Node's,
  -- or rather specific points in the 'domain' of each 'Node'.
  , referencedPoints :: !(IORef (IntArgsMonoSet (Products (ParamTypes domain))))
  -- ^ Constant (but the the wrapped set is stateful).
  -- The set of points the currently 'recompute'd node references so far.
  , unstable         :: !(IORef (IntArgsMonoSet (Products (ParamTypes domain))))
  -- ^ Constant (but the the wrapped queue is stateful).
  -- Unstable nodes that will be 'recompute'd by the 'work'list algorithm.
  }

initialEnv
  :: IntArgsMonoSet (Products (ParamTypes domain))
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

-- | The 'Domain' is extracted from a type parameter.
instance Datafixable domain => MonadDomain (DependencyM graph domain) where
  type Domain (DependencyM graph domain) = domain

-- | This allows us to solve @MonadDependency m => DataFlowProblem m@ descriptions
-- with 'solveProblem'.
instance (Datafixable domain, GraphRef graph) => MonadDependency (DependencyM graph domain) where
  dependOn = dependOn @domain @graph
  {-# INLINE dependOn #-}

-- | Specifies the /density/ of the problem, e.g. whether the domain of
-- 'Node's can be confined to a finite range, in which case 'solveProblem'
-- tries to use a "Data.Vector" based graph representation rather than
-- one based on "Data.IntMap".
data Density graph where
  Sparse :: Density SparseGraph.Ref
  Dense :: Node -> Density DenseGraph.Ref

-- | A function that computes a sufficiently conservative approximation
-- of a point in the abstract domain for when the solution algorithm
-- decides to have iterated the node often enough.
--
-- When 'domain' is a 'BoundedMeetSemilattice'/'BoundedLattice', the
-- simplest abortion function would be to constantly return 'top'.
--
-- As is the case for 'LiftedFunc' and 'ChangeDetector', this
-- carries little semantic meaning if viewed in isolation, so here
-- are a few examples for how the synonym expands:
--
-- @
--   AbortionFunction Int ~ Int -> Int
--   AbortionFunction (String -> Int) ~ String -> Int -> Int
--   AbortionFunction (a -> b -> c -> PowerSet) ~ a -> b -> c -> PowerSet -> PowerSet
-- @
--
-- E.g., the current value of the point is passed in (the tuple @(a, b, c, PowerSet)@)
-- and the function returns an appropriate conservative approximation in that
-- point.
type AbortionFunction domain
  = Arrows (ParamTypes domain) (ReturnType domain -> ReturnType domain)

-- | Aborts iteration of a value by 'const'antly returning the 'top' element
-- of the assumed 'BoundedMeetSemiLattice' of the 'ReturnType'.
abortWithTop
  :: forall domain
   . Currying (ParamTypes domain) (ReturnType domain -> ReturnType domain)
  => BoundedMeetSemiLattice (ReturnType domain)
  => AbortionFunction domain
abortWithTop =
  currys @(ParamTypes domain) @(ReturnType domain -> ReturnType domain) $
    const top
{-# INLINE abortWithTop #-}

-- | Expresses that iteration should or shouldn't stop after a point has
-- been iterated a finite number of times.
data IterationBound domain
  = NeverAbort
  -- ^ Will keep on iterating until a precise, yet conservative approximation
  -- has been reached. Make sure that your 'domain' satisfies the
  -- [ascending chain condition](https://en.wikipedia.org/wiki/Ascending_chain_condition),
  -- e.g. that fixed-point iteration always comes to a halt!
  | AbortAfter Int (AbortionFunction domain)
  -- ^ For when your 'domain' doesn't satisfy the ascending chain condition
  -- or when you are sensitive about solution performance.
  --
  -- The 'Int'eger determines the maximum number of iterations of a single point
  -- of a 'Node' (with which an entire function with many points may be associated)
  -- before iteration aborts in that point by calling the supplied 'AbortionFunction'.
  -- The responsibility of the 'AbortionFunction' is to find a sufficiently
  -- conservative approximation for the current value at that point.
  --
  -- When your 'ReturnType' is an instance of 'BoundedMeetSemiLattice',
  -- 'abortWithTop' might be a worthwhile option.
  -- A more sophisticated solution would trim the current value to a certain
  -- cut-off depth, depending on the first parameter, instead.

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
  :: State (IntArgsMonoSet (Products (ParamTypes domain))) a
  -> ReaderT (Env graph domain) IO a
zoomReferencedPoints = withReaderT referencedPoints . zoomIORef
{-# INLINE zoomReferencedPoints #-}

zoomUnstable
  :: State (IntArgsMonoSet (Products (ParamTypes domain))) a
  -> ReaderT (Env graph domain) IO a
zoomUnstable = withReaderT unstable . zoomIORef
{-# INLINE zoomUnstable #-}

enqueueUnstable
  :: k ~ Products (ParamTypes domain)
  => MonoMapKey k
  => Int -> k -> ReaderT (Env graph domain) IO ()
enqueueUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.insert i k))
{-# INLINE enqueueUnstable #-}

deleteUnstable
  :: k ~ Products (ParamTypes domain)
  => MonoMapKey k
  => Int -> k -> ReaderT (Env graph domain) IO ()
deleteUnstable i k = zoomUnstable (modify' (IntArgsMonoSet.delete i k))
{-# INLINE deleteUnstable #-}

highestPriorityUnstableNode
  :: k ~ Products (ParamTypes domain)
  => MonoMapKey k
  => ReaderT (Env graph domain) IO (Maybe (Int, k))
highestPriorityUnstableNode = zoomUnstable $
  listToMaybe . IntArgsMonoSet.highestPriorityNodes <$> get
{-# INLINE highestPriorityUnstableNode #-}

withCall
  :: Datafixable domain
  => Int
  -> Products (ParamTypes domain)
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

-- | The first of the two major functions of this module.
--
-- @recompute node args@ iterates the value of the passed @node@
-- at the point @args@ by invoking its transfer function.
-- It does so in a way that respects the 'IterationBound'.
--
-- This function is not exported, and is only called by 'work'
-- and 'dependOn', for when the iteration strategy decides that
-- the @node@ needs to be (and can be) re-iterated.
-- It performs tracking of which 'Node's the transfer function
-- depended on, do that the worklist algorithm can do its magic.
recompute
  :: forall domain graph dom cod depm
   . dom ~ ParamTypes domain
  => cod ~ ReturnType domain
  => depm ~ DependencyM graph domain
  => GraphRef graph
  => Datafixable domain
  => Int -> Products dom -> ReaderT (Env graph domain) IO cod
recompute node args = withCall node args $ do
  prob <- asks problem
  let node' = Node node
  let DM iterate' = uncurrys @dom @(depm cod) (dfpTransfer prob node') args
                  \\ lfInst @domain @depm
  let detectChange' = uncurrys @dom @(cod -> cod -> Bool) (dfpDetectChange prob node') args
                    \\ cdInst @domain
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
    return (uncurrys @dom @(cod -> cod) abort args oldVal \\ afInst @domain)
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
  :: forall domain graph depm
   . depm ~ DependencyM graph domain
  => Datafixable domain
  => GraphRef graph
  => Node -> LiftedFunc domain depm
dependOn (Node node) =
  currys @(ParamTypes domain) @(depm (ReturnType domain)) impl
    \\ lfInst @domain @(DependencyM graph domain)
    where
      impl args = DM $ do
        cycleDetected <- IntArgsMonoSet.member node args <$> asks callStack
        isStable <- zoomUnstable $
          not . IntArgsMonoSet.member node args <$> get
        maybePointInfo <- withReaderT graph (Graph.lookup node args)
        zoomReferencedPoints (modify' (IntArgsMonoSet.insert node args))
        case maybePointInfo >>= value of
          -- 'value' can only be 'Nothing' if there was a 'cycleDetected':
          -- Otherwise, the node wasn't part of the call stack and thus will either
          -- have a 'value' assigned or will not have been discovered at all.
          Nothing | cycleDetected ->
            -- Somewhere in an outer activation record we already compute this one.
            -- We don't recurse again and just return an optimistic approximation,
            -- such as 'bottom'.
            -- Otherwise, 'recompute' will immediately add a 'PointInfo' before
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
  => Datafixable domain
  => Int -> Products (ParamTypes domain) -> ReaderT (Env graph domain) IO (ReturnType domain)
optimisticApproximation node args = do
  points <- withReaderT graph (Graph.lookupLT node args)
  -- Note that 'points' might contain 'PointInfo's that have no 'value'.
  -- It's OK to filter these out: At worst, the approximation will be
  -- more optimistic than necessary.
  return (joins (mapMaybe (value . snd) points))

scheme1, scheme2
  :: GraphRef graph
  => Datafixable domain
  => Maybe (ReturnType domain)
  -> Int
  -> Products (ParamTypes domain)
  -> ReaderT (Env graph domain) IO (ReturnType domain)
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

-- There used to be a third scheme that is no longer possible with the current
-- mode of dependency tracking.
-- See https://github.com/sgraf812/journal/blob/09f0521dbdf53e7e5777501fc868bb507f5ceb1a/datafix.md.html#how-an-algorithm-that-can-do-3-looks-like

-- |As long as the supplied "Maybe" expression returns "Just _", the loop
-- body will be called and passed the value contained in the 'Just'.  Results
-- are discarded.
--
-- Taken from 'Control.Monad.Loops.whileJust_'.
whileJust_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ cond action = go
  where
    go = cond >>= \case
      Nothing -> return ()
      Just a  -> action a >> go
{-# INLINE whileJust_ #-}

-- | Defined as 'work = whileJust_ highestPriorityUnstableNode (uncurry recompute)'.
--
-- Tries to dequeue the 'highestPriorityUnstableNode' and 'recompute's the value of
-- one of its 'unstable' points, until the worklist is empty, indicating that a
-- fixed-point has been reached.
work
  :: GraphRef graph
  => Datafixable domain
  => ReaderT (Env graph domain) IO ()
work = whileJust_ highestPriorityUnstableNode (uncurry recompute)
{-# INLINE work #-}

-- | Computes the (pure) solution of the 'DependencyM' action @act@ specified in
-- the last parameter. @act@ may reference (via 'dependOn') 'Node's of the
-- 'DataFlowProblem' @dfp@'s fixed-point, specified as the first parameter.
--
-- @dfp@'s fixed-point is determined by its transfer functions, and
-- @solveProblem@ will make sure that all (relevant) 'Node's will have reached
-- their fixed-point according to these transfer function before computing the
-- solution for @act@.
--
-- We try to be smart in saving unnecessary iterations of transfer functions by
-- employing a worklist algorithm. For function domains, where each Node denotes
-- a monotone function, each points dependencies' will be tracked individually.
--
-- Apart from @dfp@ and @act@, the 'Density' of the data-flow graph and the
-- 'IterationBound' can be specified. Pass 'Sparse' and 'NeverAbort' when in
-- doubt.
--
-- If your problem only has finitely many different 'Node's , consider using the
-- 'ProblemBuilder' API (e.g. 'datafix' + 'evalDenotation') for a higher-level
-- API that let's you forget about 'Node's and instead let's you focus on
-- building more complex data-flow frameworks.
solveProblem
  :: forall domain graph a
   . GraphRef graph
  => Datafixable domain
  => DataFlowProblem (DependencyM graph domain)
  -- ^ The description of the @DataFlowProblem@.
  -> Density graph
  -- ^ Describes if the algorithm is free to use a 'Dense', 'Vector'-based
  -- graph representation or has to go with a 'Sparse' one based on 'IntMap'.
  -> IterationBound domain
  -- ^ Whether the solution algorithm should respect a maximum bound on the
  -- number of iterations per point. Pass 'NeverAbort' if you don't care.
  -> DependencyM graph domain a
  -- ^ The action for which we want to compute the solution. May reference
  -- 'Node's from the @DataFlowProblem@. If you just want to know the value of
  -- 'Node' 42, use `dependOn @(DependecyM _ domain) (Node 42)`.
  -> a
solveProblem prob density ib (DM act) = unsafePerformIO $ do
  -- Trust me, I'm an engineer! See the docs of the 'DM' constructor
  -- of 'DependencyM' for why we 'unsafePerformIO'.
  let newGraphRef = case density of
        Sparse               -> SparseGraph.newRef
        Dense (Node maxNode) -> DenseGraph.newRef (maxNode + 1)
  env <- initialEnv IntArgsMonoSet.empty prob ib newGraphRef
  -- Run act once to discover dependencies and an additional time when all
  -- values reached a fixed-point.
  runReaderT (act >> work >> act) env
{-# INLINE solveProblem #-}
