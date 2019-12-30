{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :  Datafix.Denotational
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- Provides an alternative method (to 'MonadDependency'/"Datafix.Explicit")
-- of formulating data-flow problems as a 'Denotation' built in the context of
-- 'MonadDatafix'. This offers better usability for defining static analyses,
-- as the problem of allocating nodes in the data-flow graph is abstracted from
-- the user.

module Datafix.Denotational
  ( MonadDatafix (..)
  , datafixEq
  , Denotation
  ) where

import           Datafix.Common
import           Datafix.Entailments
import           Datafix.Utils.Constraints
import           Datafix.Utils.TypeLevel

-- | Builds on an associated 'DepM' that is a 'MonadDomain' (like any
-- 'MonadDependency') by providing a way to track dependencies without explicit
-- 'Node' management. Essentially, this allows to specify a build plan for a
-- 'DataFlowProblem' through calls to 'datafix' in analogy to 'fix' or 'mfix'.
class (Monad m, MonadDomain (DepM m)) => MonadDatafix m where
  -- | The monad in which data dependencies are expressed.
  -- Can and will be instantiated to some 'MonadDependency', if you choose
  -- to go through 'ProblemBuilder'.
  type DepM m :: * -> *
  -- | This is the closest we can get to an actual fixed-point combinator.
  --
  -- We need to provide a 'ChangeDetector' for detecting the fixed-point as
  -- well as a function to be iterated. In addition to returning a better
  -- approximation of itself in terms of itself, it can return an arbitrary
  -- value of type @a@. Because the iterated function might want to 'datafix'
  -- additional times (think of nested let bindings), the return values are
  -- wrapped in @m@.
  --
  -- Finally, the arbitrary @a@ value is returned, in analogy to @a@ in
  -- @'Control.Monad.Fix.mfix' :: MonadFix m => (a -> m a) -> m a@.
  datafix
    :: dom ~ Domain (DepM m)
    => ChangeDetector dom
    -> (LiftedFunc dom (DepM m) -> m (a, LiftedFunc dom (DepM m)))
    -> m a

-- | Shorthand that partially applies 'datafix' to an 'eqChangeDetector'.
datafixEq
  :: forall m dom a
   . MonadDatafix m
  => dom ~ Domain (DepM m)
  => Eq (ReturnType dom)
  => (LiftedFunc dom (DepM m) -> m (a, LiftedFunc dom (DepM m)))
  -> m a
datafixEq = datafix @m (eqChangeDetector @dom) \\ cdInst @dom

-- | A denotation of some syntactic entity in a semantic @domain@, built in a
-- some 'MonadDatafix' context.
type Denotation domain func
  =  forall m. (MonadDatafix m, domain ~ Domain (DepM m)) => m (LiftedFunc func (DepM m))
