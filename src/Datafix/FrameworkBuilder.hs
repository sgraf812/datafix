{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      :  Datafix.FrameworkBuilder
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Builds a 'DataFlowFramework' for a 'Denotation'al formulation in terms of
-- 'MonadDatafix'. Effectively reduces descriptions from "Datafix.Denotational"
-- to ones from "Datafix.Explicit", so that solvers such as "Datafix.Worklist"
-- only have to provide an interpreter for 'MonadDependency'.

module Datafix.FrameworkBuilder
  ( FrameworkBuilder
  , buildFramework
  ) where

import           Data.Primitive.Array
import           Datafix.Common
import           Datafix.Denotational
import           Datafix.Explicit
import           Datafix.NodeAllocator

-- | Constructs a build plan for a 'DataFlowFramework' by tracking allocation of
-- 'Node's mapping to 'ChangeDetector's and transfer functions.
newtype FrameworkBuilder m a
  = FrameworkBuilder { unwrapFrameworkBuilder :: NodeAllocator (ChangeDetector (Domain m), LiftedFunc (Domain m) m) a }
  deriving (Functor, Applicative, Monad)

instance MonadDependency m => MonadDatafix (FrameworkBuilder m) where
  type DepM (FrameworkBuilder m) = m
  datafix cd func = FrameworkBuilder $ allocateNode $ \node -> do
    let deref = dependOn @m node
    (ret, transfer) <- unwrapFrameworkBuilder (func deref)
    return (ret, (cd, transfer))

-- | @(root, max, dff) = buildFramework builder@ executes the build plan specified
-- by @builder@ and returns the resulting 'DataFlowFramework' @dff@, as well as
-- the @root@ 'Node' denoting the transfer function returned by the
-- 'FrameworkBuilder' action and the @max@imum node of the problem as a proof for
-- its denseness.
buildFramework
  :: forall m a
   . MonadDependency m
  => (forall md . (MonadDatafix md, DepM md ~ m) => md a)
  -> (a, Node, DataFlowFramework m)
buildFramework plan = (a, Node (sizeofArray arr - 1), prob)
  where
    prob = DFF (snd . indexArray arr . unwrapNode) (fst . indexArray arr . unwrapNode)
    (a, arr) = runAllocator $ unwrapFrameworkBuilder $ plan @(FrameworkBuilder m)
