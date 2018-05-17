{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :  Datafix.Utils.Constraints
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Universally quantified constraints, until we have -XQuantifiedConstraints.

module Datafix.Utils.Constraints
  ( Dict (..)
  , (:-) (..)
  , (\\)
  , Forall
  , inst
  ) where

import           Data.Kind
import           Unsafe.Coerce ( unsafeCoerce )

data Dict :: Constraint -> Type where
  Dict :: c => Dict c

newtype a :- b = Sub (a => Dict b)

infixl 1 \\ -- required comment

-- | Given that @a :- b@, derive something that needs a context @b@, using the context @a@
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

-- The `Skolem` type family represents skolem variables; do not export!
-- If GHC supports it, these might be made closed with no instances.

type family Skolem (p :: k -> Constraint) :: k

-- The outer `Forall` type family prevents GHC from giving a spurious
-- superclass cycle error.
-- The inner `Forall_` class prevents the skolem from leaking to the user,
-- which would be disastrous.

-- | A representation of the quantified constraint @forall a. p a@.
type family Forall (p :: k -> Constraint) :: Constraint
type instance Forall p = Forall_ p
class p (Skolem p) => Forall_ (p :: k -> Constraint)
instance p (Skolem p) => Forall_ (p :: k -> Constraint)

inst :: forall p a . Forall p :- p a
inst = unsafeCoerce (Sub Dict :: Forall p :- p (Skolem p))