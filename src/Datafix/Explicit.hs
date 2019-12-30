{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :  Datafix.Explicit
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Primitives for describing a [data-flow problem](https://en.wikipedia.org/wiki/Data-flow_analysis) in a declarative manner.
-- This module requires you to manage assignment of 'Node's in the data-flow
-- graph to denotations by hand. If you're looking for a safer
-- approach suited for static analysis, have a look at "Datafix.Denotational".
--
-- Import this module transitively through "Datafix" and get access to
-- "Datafix.Worklist" for functions that compute solutions to your
-- 'DataFlowFramework's.

module Datafix.Explicit
  ( Node (..)
  , DataFlowFramework (..)
  , MonadDependency (..)
  ) where

import           Datafix.Common

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
  = Node { unwrapNode :: Int }
  deriving (Eq, Ord, Show)

-- | Models a data-flow problem, where each 'Node' is mapped to
-- its denoting 'LiftedFunc' and a means to detect when
-- the iterated transfer function reached a fixed-point through
-- a 'ChangeDetector'.
data DataFlowFramework m
  = DFF
  { dffTransfer     :: !(Node -> LiftedFunc (Domain m) m)
  -- ^ A transfer function per each 'Node' of the modeled data-flow problem.
  , dffDetectChange :: !(Node -> ChangeDetector (Domain m))
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
--   transferFib :: forall m . (MonadDependency m, Domain m ~ Int) => Node -> LiftedFunc Int m
--   transferFib (Node 0) = return 0
--   transferFib (Node 1) = return 1
--   transferFib (Node n) = (+) <$> dependOn @m (Node (n-1)) <*> dependOn @m (Node (n-2))
--   -- sparing the negative n error case
-- :}
--
-- We can construct a description of a 'DataFlowFramework' with this @transferFib@ function:
--
-- >>> :{
--   DataFlowFramework :: forall m . (MonadDependency m, Domain m ~ Int) => DataFlowFramework m
--   DataFlowFramework = DFF transferFib (const (eqChangeDetector @(Domain m)))
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
class MonadDomain m => MonadDependency m where
  dependOn :: Node -> LiftedFunc (Domain m) m
  -- ^ Expresses a dependency on a node of the data-flow graph, thus
  -- introducing a way of trackable recursion. That's similar
  -- to how you would use 'Data.Function.fix' to abstract over recursion.
