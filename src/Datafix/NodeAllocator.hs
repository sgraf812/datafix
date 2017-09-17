{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Datafix.NodeAllocator where

import           Control.Monad.Fix                (mfix)
import           Control.Monad.Trans.State.Strict
import           Data.IntMap.Lazy                 (IntMap)
import qualified Data.IntMap.Lazy                 as IntMap
import           Datafix

-- | A state monad wrapping a mapping from 'GraphNode' to some 'v'
-- which we will instantiate to appropriate 'TransferFunction's.
newtype NodeAllocator v a
  = NodeAllocator { unwrapNodeAllocator :: State (IntMap v) a }
  deriving (Functor, Applicative, Monad)

-- | Allocates the next 'GraphNode', which is greater than any
-- nodes requested before.
--
-- The value stored at that node is the result of a 'NodeAllocator'
-- computation which may already access the 'GraphNode' associated
-- with that value. This is important for the case of recursive
-- let, where the denotation of an expression depends on itself.
allocateNode :: (GraphNode -> NodeAllocator v (a, v)) -> NodeAllocator v a
allocateNode f = NodeAllocator $ do
  node <- gets IntMap.size
  (result, _) <- mfix $ \ ~(_, entry) -> do
    let overwriteError = error ("Overwriting allocated node entry " ++ show node)
    modify' (IntMap.insertWith overwriteError node entry)
    unwrapNodeAllocator (f (GraphNode node))
  return result

-- | Runs the allocator, beginning with an empty mapping.
runAllocator :: NodeAllocator v a -> (a, IntMap v)
runAllocator (NodeAllocator alloc) = runState alloc IntMap.empty
