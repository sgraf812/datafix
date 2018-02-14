-- |
-- Module      :  Datafix.Worklist
-- Copyright   :  (c) Sebastian Graf 2017
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- This module provides the 'Impl.fixProblem' function, which solves the description of a
-- 'Datafix.Description.DataFlowProblem' by employing a worklist algorithm.

module Datafix.Worklist
  ( Impl.DependencyM
  , Impl.Datafixable
  , Impl.IterationBound (..)
  , Impl.compile
  , Impl.fixProblem
  ) where

import qualified Datafix.Worklist.Internal as Impl
