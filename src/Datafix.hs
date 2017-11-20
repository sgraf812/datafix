{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Datafix
  ( Node (..)
  , TransferFunction
  , ChangeDetector
  , DataFlowProblem (..)
  , MonadDependency (..)
  , eqChangeDetector
  , alwaysChangeDetector
  ) where

import           Data.Proxy              (Proxy (..))
import           Datafix.Utils.TypeLevel

-- $setup

-- | This is the type we use to index nodes in the data-flow graph.
--
-- The connection between syntactic things (e.g. 'Id's) and 'Node's is
-- made implicitly in code in analysis templates through an appropriate
-- allocation mechanism as in 'NodeAllocator'.
newtype Node
  = Node Int
  deriving (Eq, Ord, Show)

-- | A function that checks points of some function with type 'domain' for changes.
-- If this returns 'True', the point of the function is assumed to have changed.
--
-- An example is worth a thousand words, especially because of the type-level hackery:
--
-- >>> cd = (\a b -> even a /= even b) :: ChangeDetector Int
--
-- This checks the parity for changes in the abstract domain of integers.
-- Integers of the same parity are considered unchanged.
--
-- >>> cd 4 5
-- True
-- >>> cd 7 13
-- False
--
-- Now a (quite bogus) pointwise example:
--
-- >>> cd = (\x fx gx -> x + abs fx /= x + abs gx) :: ChangeDetector (Int -> Int)
-- >>> cd 1 (-1) 1
-- False
-- >>> cd 15 1 2
-- True
-- >>> cd 13 35 (-35)
-- False
--
-- This would consider functions @id@ and @negate@ unchanged, so the sequence
-- @iterate negate :: Int -> Int@ would be regarded immediately as convergent:
--
-- >>> f x = iterate negate x !! 0
-- >>> let g x = iterate negate x !! 1
-- >>> cd 123 (f 123) (g 123)
-- False
type ChangeDetector domain
  = Arrows (Domains domain) (CoDomain domain -> CoDomain domain -> Bool)

-- | Goal:
-- @
--   TransferFunction m Int = m Int
--   TransferFunction m (Bool -> Int) = Bool -> m Int
--   TransferFunction m (a -> b -> Int) = a -> b -> m Int
--   TransferFunction m (a -> b -> c -> Int) = a -> b -> c -> m Int
-- @
type TransferFunction m domain
  = Arrows (Domains domain) (m (CoDomain domain))

-- | Models a data-flow problem, where each 'Node' is mapped to
-- its denoting transfer function and a means to detect when
-- the iterated transfer function reached a fixed-point.
data DataFlowProblem m
  = DFP
  { dfpTransfer     :: !(Node -> TransferFunction m (Domain m))
  -- ^ A transfer function per each 'Node' of the modeled data-flow problem.
  , dfpDetectChange :: !(Node -> ChangeDetector (Domain m))
  -- ^ A 'ChangeDetector' for each 'Node' of the modeled data-flow problem.
  -- In the simplest case, this just delegates to an 'Eq' instance.
  }

class Monad m => MonadDependency m where
  type Domain m :: *
  dependOn :: Proxy m -> Node -> TransferFunction m (Domain m)

-- | A 'ChangeDetector' that delegates to the 'Eq' instance of the
-- node values.
eqChangeDetector
  :: forall m
   . Currying (Domains (Domain m)) (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)
  => Eq (CoDomain (Domain m))
  => Proxy m
  -> ChangeDetector (Domain m)
eqChangeDetector _ =
  currys (Proxy :: Proxy (Domains (Domain m))) (Proxy :: Proxy (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)) $
    const (/=)
{-# INLINE eqChangeDetector #-}

-- | A 'ChangeDetector' that always returns 'True'.
--
-- Use this when recomputing a node is cheaper than actually testing for the change.
-- Beware of cycles in the resulting dependency graph, though!
alwaysChangeDetector
  :: forall m
   . Currying (Domains (Domain m)) (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)
  => Proxy m -> ChangeDetector (Domain m)
alwaysChangeDetector _ =
  currys (Proxy :: Proxy (Domains (Domain m))) (Proxy :: Proxy (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)) $
    \_ _ _ -> True
{-# INLINE alwaysChangeDetector #-}
