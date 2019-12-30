{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Datafix.NodeAllocator
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Helpers for allocating 'Node's in an ergonomic manner, e.g.
-- taking care to get 'mfix' right under the hood for allocation
-- in recursive bindings groups through the key primitive 'allocateNode'.

module Datafix.NodeAllocator
  ( NodeAllocator
  , allocateNode
  , runAllocator
  ) where

import           Control.Monad.Fix                (mfix)
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Primitive.Array
import           Datafix.Explicit
import           Datafix.Utils.GrowableVector     (GrowableVector)
import qualified Datafix.Utils.GrowableVector     as GV
import           System.IO.Unsafe                 (unsafePerformIO)

-- | A state monad wrapping a mapping from 'Node' to some 'v'
-- which we will instantiate to appropriate 'LiftedFunc's.
newtype NodeAllocator v a
  = NodeAllocator { unwrapNodeAllocator :: StateT (GrowableVector (PrimState IO) v) IO a }
  deriving (Functor, Applicative, Monad)

-- | Allocates the next 'Node', which is greater than any
-- nodes requested before.
--
-- The value stored at that node is the result of a 'NodeAllocator'
-- computation which may already access the 'Node' associated
-- with that value. This is important for the case of recursive
-- let, where the denotation of an expression depends on itself.
allocateNode :: (Node -> NodeAllocator v (a, v)) -> NodeAllocator v a
allocateNode f = NodeAllocator $ do
  node <- gets GV.length
  (result, _) <- mfix $ \ ~(_, entry) -> do
    vec <- get
    lift (GV.pushBack vec entry) >>= put
    unwrapNodeAllocator (f (Node node))
  return result
{-# INLINE allocateNode #-}

-- | Runs the allocator, beginning with an empty mapping.
runAllocator :: NodeAllocator v a -> (a, Array v)
runAllocator (NodeAllocator alloc) = unsafePerformIO $ do
  vec <- GV.new 8
  (a, vec') <- runStateT alloc vec
  vec'' <- GV.freeze vec'
  return (a, vec'')
{-# INLINE runAllocator #-}
