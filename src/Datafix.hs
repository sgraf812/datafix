{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Datafix
-- Copyright   :  (c) Sebastian Graf 2017
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- This is the top-level, import-all, kitchen sink module.
--
-- We'll do a little tutorial here, while we are at it!

module Datafix
  ( module Datafix.Description
  , module Datafix.NodeAllocator
  , Datafix.MonoMap.MonoMap
  , module Datafix.Utils.TypeLevel
  , module Datafix.Worklist
  ) where

import           Datafix.Description
import           Datafix.MonoMap
import           Datafix.NodeAllocator
import           Datafix.Utils.TypeLevel
import           Datafix.Worklist
