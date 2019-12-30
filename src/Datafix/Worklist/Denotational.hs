{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}

-- |
-- Module      :  Datafix.Worklist.Denotational
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Bridges the "Datafix.Worklist" solver for 'DataFlowFramework's ('solveProblem')
-- with the "Datafix.Denotational" approach, using 'MonadDatafix' to describe
-- a 'Denotation'.

module Datafix.Worklist.Denotational
  ( evalDenotation
  ) where

import           Datafix.Common
import           Datafix.Denotational
import           Datafix.Entailments
import           Datafix.Utils.Constraints
import           Datafix.Utils.TypeLevel
import           Datafix.FrameworkBuilder
import qualified Datafix.Worklist.Graph.Dense     as DenseGraph
import           Datafix.Worklist.Internal
import           Data.Type.Equality

-- | @evalDenotation denot ib@ returns a value in @domain@ that is described by
-- the denotation @denot@.
--
-- It does so by building up the 'DataFlowFramework' corresponding to @denot@
-- and solving the resulting problem with 'solveProblem', the documentation of
-- which describes in detail how to arrive at a stable denotation and what
-- the 'IterationBound' @ib@, domain ~ Domain (DepM m) is for.
evalDenotation
  :: forall domain func
   . Datafixable domain
  => Forall (Currying (ParamTypes func))
  => Denotation domain func
  -- ^ A build plan for computing the denotation, possibly involving
  -- fixed-point iteration factored through calls to 'datafix'.
  -> IterationBound domain
  -- ^ Whether the solution algorithm should respect a maximum bound on the
  -- number of iterations per point. Pass 'NeverAbort' if you don't care.
  -> func
evalDenotation plan ib =
  castWith arrowsAxiom (currys @(ParamTypes func) @(ReturnType func) impl \\ idInst @func)
    where
      impl :: Products (ParamTypes func) -> ReturnType func
      impl args = solveProblem prob (Dense max_) ib (uncurriedDenot args)
      uncurriedDenot = uncurrys @(ParamTypes func) denot \\ lfInst @func @(DependencyM DenseGraph.Ref domain)
      (denot, max_, prob) = buildFramework plan
{-# INLINE evalDenotation #-}
