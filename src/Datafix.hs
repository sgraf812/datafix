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

-- | This is the type we use to index nodes in the data-flow graph.
--
-- The connection between syntactic things (e.g. 'Id's) and 'Node's is
-- made implicitly in code in analysis templates through an appropriate
-- allocation mechanism as in 'NodeAllocator'
newtype Node
  = Node Int
  deriving (Eq, Ord, Show)

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

data DataFlowProblem m
  = DFP
  { transfer     :: !(Node -> TransferFunction m (Domain m))
  , detectChange :: !(Node -> ChangeDetector (Domain m))
  }

class Monad m => MonadDependency m where
  type Domain m :: *
  dependOn :: Proxy m -> Node -> TransferFunction m (Domain m)

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

alwaysChangeDetector
  :: forall m
   . Currying (Domains (Domain m)) (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)
  => Proxy m -> ChangeDetector (Domain m)
alwaysChangeDetector _ =
  currys (Proxy :: Proxy (Domains (Domain m))) (Proxy :: Proxy (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)) $
    \_ _ _ -> True
{-# INLINE alwaysChangeDetector #-}
