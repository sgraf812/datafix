-- This is literally
-- https://github.com/agda/agda/blob/0aff32aa29652db1a7026f81bc57dc15d5930124/src/full/Agda/Utils/TypeLevel.hs
-- with some default-extensions added.
-- Let's just hope that they don't sue ;)

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- We need undecidable instances for the definition of @Foldr@,
-- and @ParamTypes@ and @ReturnType@ using @If@ for instance.
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      :  Datafix.Utils.TypeLevel
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Some type-level helpers for 'curry'/'uncurry'ing arbitrary function types.
module Datafix.Utils.TypeLevel where

import           Data.Type.Equality
import           GHC.Exts           (Constraint)
import           Unsafe.Coerce      (unsafeCoerce)

------------------------------------------------------------------
-- CONSTRAINTS
------------------------------------------------------------------

-- | @All p as@ ensures that the constraint @p@ is satisfied by
--   all the 'types' in @as@.
--   (Types is between scare-quotes here because the code is
--   actually kind polymorphic)

type family All (p :: k -> Constraint) (as :: [k]) :: Constraint where
  All p '[]       = ()
  All p (a ': as) = (p a, All p as)

------------------------------------------------------------------
-- FUNCTIONS
-- Type-level and Kind polymorphic versions of usual value-level
-- functions.
------------------------------------------------------------------

-- | On Booleans
type family If (b :: Bool) (l :: k) (r :: k) :: k where
  If 'True  l r = l
  If 'False l r = r

-- | On Lists
type family Foldr (c :: k -> l -> l) (n :: l) (as :: [k]) :: l where
  Foldr c n '[]       = n
  Foldr c n (a ': as) = c a (Foldr c n as)

-- | Version of @Foldr@ taking a defunctionalised argument so
--   that we can use partially applied functions.
type family Foldr' (c :: Function k (Function l l -> *) -> *)
                   (n :: l) (as :: [k]) :: l where
  Foldr' c n '[]       = n
  Foldr' c n (a ': as) = Apply (Apply c a) (Foldr' c n as)

type family Map (f :: Function k l -> *) (as :: [k]) :: [l] where
  Map f as = Foldr' (ConsMap0 f) '[] as

data ConsMap0 :: (Function k l -> *) -> Function k (Function [l] [l] -> *) -> *
data ConsMap1 :: (Function k l -> *) -> k -> Function [l] [l] -> *
type instance Apply (ConsMap0 f)    a = ConsMap1 f a
type instance Apply (ConsMap1 f a) tl = Apply f a ': tl

type family Constant (b :: l) (as :: [k]) :: [l] where
  Constant b as = Map (Constant1 b) as

------------------------------------------------------------------
-- TYPE FORMERS
------------------------------------------------------------------

-- | @Arrows [a1,..,an] r@ corresponds to @a1 -> .. -> an -> r@
type Arrows   (as :: [*]) (r :: *) = Foldr (->) r as

arrowsAxiom :: Arrows (ParamTypes func) (ReturnType func) :~: func
arrowsAxiom = unsafeCoerce Refl

-- | @Products []@ corresponds to @()@,
-- @Products [a]@ corresponds to @a@,
-- @Products [a1,..,an]@ corresponds to @(a1, (..,( an)..))@.
--
-- So, not quite a right fold, because we want to optimize for the
-- empty, singleton and pair case.
type family Products (as :: [*]) where
  Products '[]       = ()
  Products '[a]      = a
  Products (a ': as) = (a, Products as)

-- | @IsBase t@ is @'True@ whenever @t@ is *not* a function space.

type family IsBase (t :: *) :: Bool where
  IsBase (a -> t) = 'False
  IsBase a        = 'True

-- | Using @IsBase@ we can define notions of @ParamTypes@ and @ReturnTypes@
--   which *reduce* under positive information @IsBase t ~ 'True@ even
--   though the shape of @t@ is not formally exposed

type family ParamTypes (t :: *) :: [*] where
  ParamTypes t = If (IsBase t) '[] (ParamTypes' t)
type family ParamTypes' (t :: *) :: [*] where
  ParamTypes' (a -> t) = a ': ParamTypes t

type family ReturnType (t :: *) :: * where
  ReturnType t = If (IsBase t) t (ReturnType' t)
type family ReturnType' (t :: *) :: * where
  ReturnType' (a -> t) = ReturnType t

------------------------------------------------------------------
-- TYPECLASS MAGIC
------------------------------------------------------------------

-- | @Currying as b@ witnesses the isomorphism between @Arrows as b@
--   and @Products as -> b@. It is defined as a type class rather
--   than by recursion on a singleton for @as@ so all of that these
--   conversions are inlined at compile time for concrete arguments.

class Currying as b where
  uncurrys :: Arrows as b -> Products as -> b
  currys   :: (Products as -> b) -> Arrows as b

instance Currying '[] b where
  uncurrys f () = f
  currys   f = f ()

instance Currying (a ': '[]) b where
  uncurrys f = f
  currys f = f

instance Currying (a2 ': as) b => Currying (a1 ': a2 ': as) b where
  uncurrys f = uncurry $ uncurrys @(a2 ': as) . f
  currys   f = currys @(a2 ': as) . curry f

------------------------------------------------------------------
-- DEFUNCTIONALISATION
-- Cf. Eisenberg and Stolarek's paper:
-- Promoting Functions to Type Families in Haskell
------------------------------------------------------------------

data Function :: * -> * -> *

data Constant0 :: Function a (Function b a -> *) -> *
data Constant1 :: * -> Function b a -> *

type family Apply (t :: Function k l -> *) (u :: k) :: l

type instance Apply Constant0     a = Constant1 a
type instance Apply (Constant1 a) b = a
