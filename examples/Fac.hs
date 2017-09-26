{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Fac where

import           Data.Proxy
import           Datafix
import           Numeric.Natural

facProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowProblem m
facProblem = DFP transfer (const (eqChangeDetector p))
  where
    p :: Proxy m
    p = Proxy
    transfer :: Node -> TransferFunction m Natural
    transfer (Node 0) = return 1
    transfer (Node 1) = return 1
    transfer (Node n) = do
      a <- dependOn p (Node (n-1))
      return (fromIntegral n * a)

fac :: Int -> Natural
fac n = product [1..fromIntegral n]
