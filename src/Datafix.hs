{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Datafix
-- Copyright   :  (c) Sebastian Graf 2018
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- This is the top-level, import-all, kitchen sink module.
--
-- Look at "Datafix.Tutorial" for a tour guided by use cases.

module Datafix
  ( module Datafix.Description
  , module Datafix.NodeAllocator
  , module Datafix.ProblemBuilder
  , Datafix.MonoMap.MonoMap
  , module Datafix.Utils.TypeLevel
  , module Datafix.Worklist
  ) where

import           Datafix.Description
import           Datafix.MonoMap
import           Datafix.NodeAllocator
import           Datafix.ProblemBuilder
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist
