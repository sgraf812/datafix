{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Fib where

import           Data.Proxy
import           Datafix
import           Numeric.Natural

fibProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowProblem m
fibProblem = DFP transfer (const (eqChangeDetector p))
  where
    p :: Proxy m
    p = Proxy
    transfer :: Node -> TransferFunction m Natural
    transfer (Node 0) = return 0
    transfer (Node 1) = return 1
    transfer (Node n) = do
      a <- dependOn p (Node (n-1))
      b <- dependOn p (Node (n-2))
      return (a + b)

fib :: Int -> Natural
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
