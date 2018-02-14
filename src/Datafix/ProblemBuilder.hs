{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
-- Offers an instance for 'MonadDatafix' based on 'NodeAllocator'.

module Datafix.ProblemBuilder
  ( ProblemBuilder
  , buildProblem
  ) where

import           Data.Primitive.Array
import           Datafix.Description
import           Datafix.NodeAllocator
import           Datafix.Utils.TypeLevel

-- | Constructs a build plan for a 'DataFlowProblem' by tracking allocation of
-- 'Node's mapping to 'ChangeDetector's and transfer functions.
newtype ProblemBuilder m a
  = ProblemBuilder { unwrapProblemBuilder :: NodeAllocator (ChangeDetector (Domain m), LiftedFunc (Domain m) m) a }
  deriving (Functor, Applicative, Monad)

instance MonadDependency m => MonadDatafix m (ProblemBuilder m) where
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
  :: forall m
   . MonadDependency m
  => Currying (ParamTypes (Domain m)) (ReturnType (Domain m) -> ReturnType (Domain m) -> Bool)
  => ProblemBuilder m (LiftedFunc (Domain m) m)
  -> (Node, Node, DataFlowProblem m)
buildProblem buildDenotation = (root, Node (sizeofArray arr - 1), prob)
  where
    prob = DFP (snd . indexArray arr . unwrapNode) (fst . indexArray arr . unwrapNode)
    (root, arr) = runAllocator $ allocateNode $ \root_ -> do
      denotation <- unwrapProblemBuilder buildDenotation
      return (root_, (alwaysChangeDetector @(Domain m), denotation))
