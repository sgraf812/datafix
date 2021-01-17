{-# LANGUAGE CPP #-}

-- |
-- Module      :  Datafix.Utils.SemiLattice
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Type classes for Semi-lattices that are no longer imported from @lattices@
-- since version 2.0. Only stuff we need.

module Datafix.Utils.SemiLattice
  ( JoinSemiLattice(..), joins
  , BoundedJoinSemiLattice(..)
  , MeetSemiLattice(..), meets
  , BoundedMeetSemiLattice(..)
  ) where

#if !MIN_VERSION_lattices(2,0,0)

-- Just re-export the old API
import           Algebra.Lattice

#else

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Void

-- Below is copy and pasted from
-- https://github.com/haskellari/lattices/blob/2e388f868b41e75a18f15442c6106bb99260e088/src/Datafix.Utils.SemiLattice.hs

infixr 6 /\ -- This comment needed because of CPP
infixr 5 \/

-- | A algebraic structure with element joins: <http://en.wikipedia.org/wiki/Semilattice>
--
-- > Associativity: x \/ (y \/ z) == (x \/ y) \/ z
-- > Commutativity: x \/ y == y \/ x
-- > Idempotency:   x \/ x == x
class JoinSemiLattice a where
    (\/) :: a -> a -> a

-- | A algebraic structure with element meets: <http://en.wikipedia.org/wiki/Semilattice>
--
-- > Associativity: x /\ (y /\ z) == (x /\ y) /\ z
-- > Commutativity: x /\ y == y /\ x
-- > Idempotency:   x /\ x == x
class MeetSemiLattice a where
    (/\) :: a -> a -> a

-- | A join-semilattice with an identity element 'bottom' for '\/'.
--
-- > Identity: x \/ bottom == x
class JoinSemiLattice a => BoundedJoinSemiLattice a where
    bottom :: a

-- | The join of a list of join-semilattice elements
joins :: (BoundedJoinSemiLattice a, Foldable f) => f a -> a
joins = foldr (\/) bottom

-- | A meet-semilattice with an identity element 'top' for '/\'.
--
-- > Identity: x /\ top == x
class MeetSemiLattice a => BoundedMeetSemiLattice a where
    top :: a

-- | The meet of a list of meet-semilattice elements
meets :: (BoundedMeetSemiLattice a, Foldable f) => f a -> a
meets = foldr (/\) top

--
-- Sets
--

instance Ord a => JoinSemiLattice (S.Set a) where
    (\/) = S.union

instance Ord a => MeetSemiLattice (S.Set a) where
    (/\) = S.intersection

instance Ord a => BoundedJoinSemiLattice (S.Set a) where
    bottom = S.empty

--
-- IntSets
--

instance JoinSemiLattice IS.IntSet where
    (\/) = IS.union

instance MeetSemiLattice IS.IntSet where
    (/\) = IS.intersection

instance BoundedJoinSemiLattice IS.IntSet where
    bottom = IS.empty

--
-- Maps
--

instance (Ord k, JoinSemiLattice v) => JoinSemiLattice (M.Map k v) where
    (\/) = M.unionWith (\/)

instance (Ord k, MeetSemiLattice v) => MeetSemiLattice (M.Map k v) where
    (/\) = M.intersectionWith (/\)

instance (Ord k, JoinSemiLattice v) => BoundedJoinSemiLattice (M.Map k v) where
    bottom = M.empty

--
-- IntMaps
--

instance JoinSemiLattice v => JoinSemiLattice (IM.IntMap v) where
    (\/) = IM.unionWith (\/)

instance JoinSemiLattice v => BoundedJoinSemiLattice (IM.IntMap v) where
    bottom = IM.empty

instance MeetSemiLattice v => MeetSemiLattice (IM.IntMap v) where
    (/\) = IM.intersectionWith (/\)

--
-- Functions
--

instance JoinSemiLattice v => JoinSemiLattice (k -> v) where
    f \/ g = \x -> f x \/ g x

instance MeetSemiLattice v => MeetSemiLattice (k -> v) where
    f /\ g = \x -> f x /\ g x

instance BoundedJoinSemiLattice v => BoundedJoinSemiLattice (k -> v) where
    bottom = const bottom

instance BoundedMeetSemiLattice v => BoundedMeetSemiLattice (k -> v) where
    top = const top

-- Unit
instance JoinSemiLattice () where
  _ \/ _ = ()

instance BoundedJoinSemiLattice () where
  bottom = ()

instance MeetSemiLattice () where
  _ /\ _ = ()

instance BoundedMeetSemiLattice () where
  top = ()

--
-- Tuples
--

instance (JoinSemiLattice a, JoinSemiLattice b) => JoinSemiLattice (a, b) where
    (x1, y1) \/ (x2, y2) = (x1 \/ x2, y1 \/ y2)

instance (MeetSemiLattice a, MeetSemiLattice b) => MeetSemiLattice (a, b) where
    (x1, y1) /\ (x2, y2) = (x1 /\ x2, y1 /\ y2)

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (a, b) where
    bottom = (bottom, bottom)

instance (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (a, b) where
    top = (top, top)

--
-- Bools
--

instance JoinSemiLattice Bool where
    (\/) = (||)

instance MeetSemiLattice Bool where
    (/\) = (&&)

instance BoundedJoinSemiLattice Bool where
    bottom = False

instance BoundedMeetSemiLattice Bool where
    top = True

-- Void
instance JoinSemiLattice Void where
  a \/ _ = a

instance MeetSemiLattice Void where
  a /\ _ = a

#endif
