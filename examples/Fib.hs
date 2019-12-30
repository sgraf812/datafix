{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Fib where

import           Datafix
import           Numeric.Natural

fibProblem :: forall m . (MonadDependency m, Domain m ~ Natural) => DataFlowFramework m
fibProblem = DFF transfer (const (eqChangeDetector @(Domain m)))
  where
    transfer :: Node -> LiftedFunc Natural m
    transfer (Node 0) = return 0
    transfer (Node 1) = return 1
    transfer (Node n) = do
      a <- dependOn @m (Node (n-1))
      b <- dependOn @m (Node (n-2))
      return (a + b)

fib :: Int -> Natural
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
