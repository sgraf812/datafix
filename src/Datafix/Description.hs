{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Datafix.Description
-- Copyright   :  (c) Sebastian Graf 2017
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Primitives for describing a [data-flow problem](https://en.wikipedia.org/wiki/Data-flow_analysis) in a declarative manner.
--
-- Import this module transitively through "Datafix" and get access to "Datafix.Worklist" for functions that compute solutions to your 'DataFlowProblem's.

module Datafix.Description
  ( Node (..)
  , TransferFunction
  , ChangeDetector
  , DataFlowProblem (..)
  , MonadDependency (..)
  , eqChangeDetector
  , alwaysChangeDetector
  ) where

import           Data.Proxy              (Proxy (..))
import           Datafix.Utils.TypeLevel

-- $setup
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Proxy
--

-- | This is the type we use to index nodes in the data-flow graph.
--
-- The connection between syntactic things (e.g. 'Id's) and 'Node's is
-- made implicitly in code in analysis templates through an appropriate
-- allocation mechanism as in 'NodeAllocator'.
newtype Node
  = Node Int
  deriving (Eq, Ord, Show)

-- | A function that checks points of some function with type 'domain' for changes.
-- If this returns 'True', the point of the function is assumed to have changed.
--
-- An example is worth a thousand words, especially because of the type-level hackery:
--
-- >>> cd = (\a b -> even a /= even b) :: ChangeDetector Int
--
-- This checks the parity for changes in the abstract domain of integers.
-- Integers of the same parity are considered unchanged.
--
-- >>> cd 4 5
-- True
-- >>> cd 7 13
-- False
--
-- Now a (quite bogus) pointwise example:
--
-- >>> cd = (\x fx gx -> x + abs fx /= x + abs gx) :: ChangeDetector (Int -> Int)
-- >>> cd 1 (-1) 1
-- False
-- >>> cd 15 1 2
-- True
-- >>> cd 13 35 (-35)
-- False
--
-- This would consider functions @id@ and @negate@ unchanged, so the sequence
-- @iterate negate :: Int -> Int@ would be regarded immediately as convergent:
--
-- >>> f x = iterate negate x !! 0
-- >>> let g x = iterate negate x !! 1
-- >>> cd 123 (f 123) (g 123)
-- False
type ChangeDetector domain
  = Arrows (ParamTypes domain) (ReturnType domain -> ReturnType domain -> Bool)

-- | Data-flow problems denote 'Node's in the data-flow graph
-- by monotone transfer functions.
--
-- This type alias alone carries no semantic meaning.
-- However, it is instructive to see some examples of how
-- this alias reduces to a normal form:
--
-- @
--   TransferFunction m Int ~ m Int
--   TransferFunction m (Bool -> Int) ~ Bool -> m Int
--   TransferFunction m (a -> b -> Int) ~ a -> b -> m Int
--   TransferFunction m (a -> b -> c -> Int) ~ a -> b -> c -> m Int
-- @
--
-- @m@ will generally be an instance of 'MonadDependency' and the type alias
-- effectively wraps @m@ around @domain@'s return type.
-- The result is a function that is produces its return value while
-- potentially triggering side-effects in @m@, which amounts to
-- depending on 'TransferFunction's of other 'Node's for the
-- 'MonadDependency' case.
type TransferFunction m domain
  = Arrows (ParamTypes domain) (m (ReturnType domain))

-- | Models a data-flow problem, where each 'Node' is mapped to
-- its denoting 'TransferFunction' and a means to detect when
-- the iterated transfer function reached a fixed-point through
-- a 'ChangeDetector'.
data DataFlowProblem m
  = DFP
  { dfpTransfer     :: !(Node -> TransferFunction m (Domain m))
  -- ^ A transfer function per each 'Node' of the modeled data-flow problem.
  , dfpDetectChange :: !(Node -> ChangeDetector (Domain m))
  -- ^ A 'ChangeDetector' for each 'Node' of the modeled data-flow problem.
  -- In the simplest case, this just delegates to an 'Eq' instance.
  }

-- | A monad with a single impure primitive 'dependOn' that expresses
-- a dependency on a 'Node' of a data-flow graph.
--
-- The associated 'Domain' type is the abstract domain in which
-- we denote 'Node's.
--
-- Think of it like memoization on steroids.
-- You can represent dynamic programs with this quite easily:
--
-- >>> :{
--   transferFib :: (MonadDependency m, Domain m ~ Int) => Proxy m -> Node -> TransferFunction m Int
--   transferFib _ (Node 0) = return 0
--   transferFib _ (Node 1) = return 1
--   transferFib p (Node n) = (+) <$> dependOn p (Node (n-1)) <*> dependOn p (Node (n-2))
--   -- sparing the negative n error case
-- :}
--
-- We can construct a description of a 'DataFlowProblem' with this @transferFib@ function:
--
-- >>> :{
--   dataFlowProblem :: forall m . (MonadDependency m, Domain m ~ Int) => DataFlowProblem m
--   dataFlowProblem = DFP (transferFib (Proxy :: Proxy m)) (const (eqChangeDetector (Proxy :: Proxy m)))
-- :}
--
-- We regard the ordinary @fib@ function a solution to the recurrence modeled by @transferFib@:
--
-- >>> :{
--   fib :: Int -> Int
--   fib 0 = 0
--   fib 1 = 1
--   fib n = fib (n-1) + fib (n - 2)
-- :}
--
-- E.g., under the assumption of @fib@ being total (which is true on the domain of natural numbers),
-- it computes the same results as the least /fixed-point/ of the series of iterations
-- of the transfer function @transferFib@.
--
-- Ostensibly, the nth iteration of @transferFib@ substitutes each @dependOn@
-- with @transferFib@ repeatedly for n times and finally substitutes all
-- remaining @dependOn@s with a call to 'error'.
--
-- Computing a solution by /fixed-point iteration/ in a declarative manner is the
-- purpose of this library. There potentially are different approaches to
-- computing a solution, but in "Datafix.Worklist" we offer an approach
-- based on a worklist algorithm, trying to find a smart order in which
-- nodes in the data-flow graph are reiterated.
--
-- The concrete MonadDependency depends on the solution algorithm, which
-- is in fact the reason why there is no satisfying data type in this module:
-- We are only concerned with /declaring/ data-flow problems here.
--
-- The distinguishing feature of data-flow graphs is that they are not
-- necessarily acyclic (data-flow graphs of dynamic programs always are!),
-- but [under certain conditions](https://en.wikipedia.org/wiki/Kleene_fixed-point_theorem)
-- even have solutions when there are cycles.
--
-- Cycles occur commonly in data-flow problems of static analyses for
-- programming languages, introduced through loops or recursive functions.
-- Thus, this library mostly aims at making the life of compiler writers
-- easier.
class Monad m => MonadDependency m where
  type Domain m :: *
  -- ^ The abstract domain in which 'Node's of the data-flow graph are denoted.
  -- When this is a synonym for a function, then all functions of this domain
  -- are assumed to be monotone wrt. the (at least) partial order of all occuring
  -- types!
  --
  -- If you can't guarantee monotonicity, try to pull non-monotone arguments
  -- into 'Node's.
  dependOn :: Proxy m -> Node -> TransferFunction m (Domain m)
  -- ^ Expresses a dependency on a node of the data-flow graph, thus
  -- introducing a way of trackable recursion. That's similar
  -- to how you would use 'Data.Function.fix' to abstract over recursion.

-- | A 'ChangeDetector' that delegates to the 'Eq' instance of the
-- node values.
eqChangeDetector
  :: forall m
   . Currying (ParamTypes (Domain m)) (ReturnType (Domain m) -> ReturnType (Domain m) -> Bool)
  => Eq (ReturnType (Domain m))
  => Proxy m
  -> ChangeDetector (Domain m)
eqChangeDetector _ =
  currys (Proxy :: Proxy (ParamTypes (Domain m))) (Proxy :: Proxy (ReturnType (Domain m) -> ReturnType (Domain m) -> Bool)) $
    const (/=)
{-# INLINE eqChangeDetector #-}

-- | A 'ChangeDetector' that always returns 'True'.
--
-- Use this when recomputing a node is cheaper than actually testing for the change.
-- Beware of cycles in the resulting dependency graph, though!
alwaysChangeDetector
  :: forall m
   . Currying (ParamTypes (Domain m)) (ReturnType (Domain m) -> ReturnType (Domain m) -> Bool)
  => Proxy m -> ChangeDetector (Domain m)
alwaysChangeDetector _ =
  currys (Proxy :: Proxy (ParamTypes (Domain m))) (Proxy :: Proxy (ReturnType (Domain m) -> ReturnType (Domain m) -> Bool)) $
    \_ _ _ -> True
{-# INLINE alwaysChangeDetector #-}
