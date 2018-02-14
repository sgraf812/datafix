-- |
-- Module      :  Datafix.Worklist
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- This module provides the 'Impl.solveProblem' function, which solves the description of a
-- 'Datafix.Description.DataFlowProblem' by employing a worklist algorithm.

module Datafix.Worklist
  ( Impl.DependencyM
  , Impl.Datafixable
  , Impl.Density (..)
  , Impl.IterationBound (..)
  , Impl.solveProblem
  , Impl.evalDenotation
  ) where

import qualified Datafix.Worklist.Internal as Impl
