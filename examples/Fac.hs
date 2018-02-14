{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Fac where

import           Datafix
import           Numeric.Natural

facProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowProblem m
facProblem = DFP transfer (const (eqChangeDetector @(Domain m)))
  where
    transfer :: Node -> LiftedFunc Natural m
    transfer (Node 0) = return 1
    transfer (Node 1) = return 1
    transfer (Node n) = do
      a <- dependOn @m (Node (n-1))
      return (fromIntegral n * a)

fac :: Int -> Natural
fac n = product [1..fromIntegral n]
