{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- |
-- Module      :  Datafix.Worklist.Denotational
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Bridges the "Datafix.Worklist" solver for 'DataFlowProblem's ('solveProblem')
-- with the "Datafix.Denotational" approach, using 'MonadDatafix' to describe
-- a 'Denotation'.

module Datafix.Worklist.Denotational
  ( evalDenotation
  ) where

import           Datafix.Common
import           Datafix.Denotational
import           Datafix.ProblemBuilder
import           Datafix.Worklist.Internal

-- | @evalDenotation denot ib@ returns a value in @domain@ that is described by
-- the denotation @denot@.
--
-- It does so by building up the 'DataFlowProblem' corresponding to @denot@
-- and solving the resulting problem with 'solveProblem', the documentation of
-- which describes in detail how to arrive at a stable denotation and what
-- the 'IterationBound' @ib@ is for.
evalDenotation
  :: Datafixable domain
  => Denotation domain
  -- ^ A build plan for computing the denotation, possibly involving
  -- fixed-point iteration factored through calls to 'datafix'.
  -> IterationBound domain
  -- ^ Whether the solution algorithm should respect a maximum bound on the
  -- number of iterations per point. Pass 'NeverAbort' if you don't care.
  -> domain
evalDenotation denot ib = solveProblem prob (Dense max_) ib root
  where
    (root, max_, prob) = buildProblem denot
{-# INLINE evalDenotation #-}
