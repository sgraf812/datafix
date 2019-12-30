{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  Datafix.Entailments
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- A bunch of helpful auxiliary entailments for 'Currying' that are recurring
-- throughout the code base.

module Datafix.Entailments
  ( cdInst
  , lfInst
  , afInst
  , idInst
  ) where

import           Datafix.Utils.Constraints
import           Datafix.Utils.TypeLevel

-- | 'Currying' entailment for 'ChangeDetector's.
cdInst
  :: Forall (Currying (ParamTypes domain))
  :- Currying (ParamTypes domain) (ReturnType domain -> ReturnType domain -> Bool)
cdInst = inst

-- | 'Currying' entailment for 'LiftedFunc's.
lfInst
  :: Forall (Currying (ParamTypes domain))
  :- Currying (ParamTypes domain) (m (ReturnType domain))
lfInst = inst

-- | 'Currying' entailment for abortion functions.
afInst
  :: Forall (Currying (ParamTypes domain))
  :- Currying (ParamTypes domain) (ReturnType domain -> ReturnType domain)
afInst = inst

-- | 'Currying' entailment for pure functions.
idInst
  :: Forall (Currying (ParamTypes domain))
  :- Currying (ParamTypes domain) (ReturnType domain)
idInst = inst
