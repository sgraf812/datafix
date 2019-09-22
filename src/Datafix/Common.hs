{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :  Datafix.Common
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Common definitions for defining data-flow problems, defining infrastructure
-- around the notion of 'Domain'.

module Datafix.Common
  ( LiftedFunc
  , ChangeDetector
  , eqChangeDetector
  , alwaysChangeDetector
  , MonadDomain (..)
  , Datafixable
  , evalAt
  , (<!)
  ) where

import           Algebra.Lattice
import           Datafix.MonoMap
import           Datafix.Utils.Constraints
import           Datafix.Utils.TypeLevel
import           Data.Type.Equality

-- $setup
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables
--

-- | Data-flow problems denote 'Node's in the data-flow graph
-- by monotone transfer functions.
--
-- This type alias alone carries no semantic meaning.
-- However, it is instructive to see some examples of how
-- this alias reduces to a normal form:
--
-- @
--   LiftedFunc Int m ~ m Int
--   LiftedFunc (Bool -> Int) m ~ Bool -> m Int
--   LiftedFunc (a -> b -> Int) m ~ a -> b -> m Int
--   LiftedFunc (a -> b -> c -> Int) m ~ a -> b -> c -> m Int
-- @
--
-- @m@ will generally be an instance of 'MonadDependency' and the type alias
-- effectively wraps @m@ around @domain@'s return type.
-- The result is a function that produces its return value while
-- potentially triggering side-effects in @m@, which amounts to
-- depending on 'LiftedFunc's of other 'Node's for the
-- 'MonadDependency' case.
type LiftedFunc domain m
  = Arrows (ParamTypes domain) (m (ReturnType domain))

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
  = Arrows (ParamTypes domain) (ReturnType domain -> ReturnType domain -> Bool)

-- | A 'ChangeDetector' that delegates to the 'Eq' instance of the
-- node values.
eqChangeDetector
  :: forall domain
   . Currying (ParamTypes domain) (ReturnType domain -> ReturnType domain -> Bool)
  => Eq (ReturnType domain)
  => ChangeDetector domain
eqChangeDetector =
  currys @(ParamTypes domain) @(ReturnType domain -> ReturnType domain -> Bool) $
    const (/=)
{-# INLINE eqChangeDetector #-}

-- | A 'ChangeDetector' that always returns 'True'.
--
-- Use this when recomputing a node is cheaper than actually testing for the change.
-- Beware of cycles in the resulting dependency graph, though!
alwaysChangeDetector
  :: forall domain
   . Currying (ParamTypes domain) (ReturnType domain -> ReturnType domain -> Bool)
  => ChangeDetector domain
alwaysChangeDetector =
  currys @(ParamTypes domain) @(ReturnType domain -> ReturnType domain -> Bool) $
    \_ _ _ -> True
{-# INLINE alwaysChangeDetector #-}

-- | A monad with an associated 'Domain'. This class exists mostly to share the
-- associated type-class between 'MonadDependency' and 'MonadDatafix'.
--
-- Also it implies that @m@ satisfies 'Datafixable', which is common enough
class (Monad m, Datafixable (Domain m)) => MonadDomain m where
  -- | The abstract domain in which nodes of the data-flow graph are denoted.
  -- When this reduces to a function, then all functions of this domain
  -- are assumed to be monotone wrt. the (at least) partial order of all occuring
  -- types!
  --
  -- If you can't guarantee monotonicity, try to pull non-monotone arguments
  -- into 'Node's.
  type Domain m :: *

-- | A constraint synonym for constraints the 'domain' has to suffice.
--
-- This is actually a lot less scary than you might think.
-- Assuming we got [quantified class constraints](http://i.cs.hku.hk/~bruno/papers/hs2017.pdf)
-- instead of hackery from the [@constraints@ package](https://hackage.haskell.org/package/constraints-0.10/docs/Data-Constraint-Forall.html#t:ForallF),
-- @Datafixable@ is a specialized version of this:
--
-- @
-- type Datafixable domain =
--   ( forall r. Currying (ParamTypes domain) r
--   , MonoMapKey (Products (ParamTypes domain))
--   , BoundedJoinSemiLattice (ReturnType domain)
--   )
-- @
--
-- Now, let's assume a concrete @domain ~ String -> Bool -> Int@, so that
-- @'ParamTypes' (String -> Bool -> Int)@ expands to the type-level list @'[String, Bool]@
-- and @'Products' '[String, Bool]@ reduces to @(String, Bool)@.
--
-- Then this constraint makes sure we are able to
--
--  1.  Curry the domain of @String -> Bool -> r@ for all @r@ to e.g. @(String, Bool) -> r@.
--      See 'Currying'. This constraint should always be discharged automatically by the
--      type-checker as soon as 'ParamTypes' and 'ReturnTypes' reduce for the 'Domain' argument,
--      which happens when the concrete @'MonadDependency' m@ is known.
--
--  2.  We want to use a [monotone](https://en.wikipedia.org/wiki/Monotonic_function)
--      map of @(String, Bool)@ to @Int@ (the @ReturnType domain@). This is
--      ensured by the @'MonoMapKey' (String, Bool)@ constraint.
--
--      This constraint has to be discharged manually, but should amount to a
--      single line of boiler-plate in most cases, see 'MonoMapKey'.
--
--      Note that the monotonicity requirement means we have to pull non-monotone
--      arguments in @Domain m@ into the 'Node' portion of the 'DataFlowProblem'.
--
--  3.  For fixed-point iteration to work at all, the values which we iterate
--      naturally have to be instances of 'BoundedJoinSemiLattice'.
--      That type-class allows us to start iteration from a most-optimistic 'bottom'
--      value and successively iterate towards a conservative approximation using
--      the '(\/)' operator.
type Datafixable domain =
  ( Forall (Currying (ParamTypes domain))
  , MonoMapKey (Products (ParamTypes domain))
  , BoundedJoinSemiLattice (ReturnType domain)
  )


evalAt
  :: forall f arr
   . Currying (ParamTypes arr) (ReturnType arr)
  => Functor f
  => f arr
  -> Products (ParamTypes arr)
  -> f (ReturnType arr)
evalAt mfunc args = app <$> mfunc
  where
    app func = uncurrys @(ParamTypes arr) (castWith (sym arrowsAxiom) func) args

(<!)
  :: forall f arr
   . Currying (ParamTypes arr) (ReturnType arr)
  => Functor f
  => f arr
  -> Products (ParamTypes arr)
  -> f (ReturnType arr)
mfunc <! args = app <$> mfunc
  where
    app func = uncurrys @(ParamTypes arr) (castWith (sym arrowsAxiom) func) args
