{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      :  Datafix.ProblemBuilder
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Builds a 'DataFlowProblem' for a 'Denotation'al formulation in terms of
-- 'MonadDatafix'. Effectively reduces descriptions from "Datafix.Denotational"
-- to ones from "Datafix.Explicit", so that solvers such as "Datafix.Worklist"
-- only have to provide an interpreter for 'MonadDependency'.

module Datafix.ProblemBuilder
  ( ProblemBuilder
  , buildProblem
  ) where

import           Data.Primitive.Array
import           Datafix.Common
import           Datafix.Denotational
import           Datafix.Explicit
import           Datafix.NodeAllocator

-- | Constructs a build plan for a 'DataFlowProblem' by tracking allocation of
-- 'Node's mapping to 'ChangeDetector's and transfer functions.
newtype ProblemBuilder m a
  = ProblemBuilder { unwrapProblemBuilder :: NodeAllocator (ChangeDetector (Domain m), LiftedFunc (Domain m) m) a }
  deriving (Functor, Applicative, Monad)

instance MonadDependency m => MonadDatafix (ProblemBuilder m) where
  type DepM (ProblemBuilder m) = m
  datafix cd func = ProblemBuilder $ allocateNode $ \node -> do
    let deref = dependOn @m node
    (ret, transfer) <- unwrapProblemBuilder (func deref)
    return (ret, (cd, transfer))

-- | @(root, max, dfp) = buildProblem builder@ executes the build plan specified
-- by @builder@ and returns the resulting 'DataFlowProblem' @dfp@, as well as
-- the @root@ 'Node' denoting the transfer function returned by the
-- 'ProblemBuilder' action and the @max@imum node of the problem as a proof for
-- its denseness.
buildProblem
  :: forall m a
   . MonadDependency m
  => (forall md . (MonadDatafix md, DepM md ~ m) => md a)
  -> (a, Node, DataFlowProblem m)
buildProblem plan = (a, Node (sizeofArray arr - 1), prob)
  where
    prob = DFP (snd . indexArray arr . unwrapNode) (fst . indexArray arr . unwrapNode)
    (a, arr) = runAllocator $ unwrapProblemBuilder $ plan @(ProblemBuilder m)
