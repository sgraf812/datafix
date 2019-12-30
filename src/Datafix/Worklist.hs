-- |
-- Module      :  Datafix.Worklist
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- This module provides the 'Impl.solveProblem' function, which solves the description of a
-- 'Datafix.Description.DataFlowProblem' by employing a worklist algorithm.
-- There's also an interpreter for 'Denotation'al problems in the form of
-- 'Denotational.evalDenotation'.

module Datafix.Worklist
  ( Impl.DependencyM
  , Impl.Density (..)
  , Impl.IterationBound (..)
  , Impl.solveProblem
  , Denotational.evalDenotation
  ) where

import qualified Datafix.Worklist.Denotational as Denotational
import qualified Datafix.Worklist.Internal     as Impl
