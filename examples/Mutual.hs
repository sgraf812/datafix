{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Mutual where

import           Data.Proxy
import           Datafix
import           Numeric.Natural

-- | A 'DataFlowProblem' with two nodes, mutually depending on another, like
--
-- @
--    a = b + 1
--    b = min a 10
-- @
--
-- After a few bounces, this will reach a stable state where the first node
-- has value 11 and the other has value 10.
mutualRecursiveProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowProblem m
mutualRecursiveProblem = DFP transfer (const (eqChangeDetector p))
  where
    p :: Proxy m
    p = Proxy
    transfer :: Node -> LiftFunc m Natural
    transfer (Node 0) = do
      b <- dependOn p (Node 1)
      return (b + 1)
    transfer (Node 1) = do
      a <- dependOn p (Node 0)
      return (min 10 a) -- So the overall fixpoint of this is 10
    transfer (Node _) = error "Invalid node"
