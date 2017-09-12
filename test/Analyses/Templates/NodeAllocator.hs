{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyses.Templates.NodeAllocator where

import           Control.Monad.Fix                (mfix)
import           Control.Monad.Trans.State.Strict
import           Data.IntMap.Lazy                 (IntMap)
import qualified Data.IntMap.Lazy                 as IntMap

-- | This is the type we use to index nodes in the data-flow graph.
--
-- The connection between 'Id's and 'ExprNode's is made implicitly
-- in code in analysis templates such as 'LetDn' in 'registerBindingGroup'.
newtype ExprNode
  = ExprNode Int
  deriving (Eq, Ord, Show)

-- | A state monad wrapping a mapping from 'ExprNode' to some 'v'
-- which we will instantiate to appropriate 'TransferFunction's.
newtype NodeAllocator v a
  = NodeAllocator { unwrapNodeAllocator :: State (IntMap v) a }
  deriving (Functor, Applicative, Monad)

-- | Allocates the next 'ExprNode', which is greater than any
-- nodes requested before.
--
-- The value stored at that node is the result of a 'NodeAllocator'
-- computation which may already access the 'ExprNode' associated
-- with that value. This is important for the case of recursive
-- let, where the denotation of an expression depends on itself.
allocateNode :: (ExprNode -> NodeAllocator v (a, v)) -> NodeAllocator v a
allocateNode f = NodeAllocator $ do
  node <- gets IntMap.size
  (result, _) <- mfix $ \ ~(_, entry) -> do
    let overwriteError = error ("Overwriting TransferFunction " ++ show node)
    modify' (IntMap.insertWith overwriteError node entry)
    unwrapNodeAllocator (f (ExprNode node))
  return result

-- | Runs the allocator, beginning with an empty mapping.
runAllocator :: NodeAllocator v a -> (a, IntMap v)
runAllocator (NodeAllocator alloc) = runState alloc IntMap.empty
