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
-- Look at "Datafix.Tutorial" for a tour guided by use cases.

module Datafix (Datafix.MonoMap.MonoMap, module X) where

import           Datafix.MonoMap
import           Datafix.Utils.TypeLevel as X
import           Datafix.Worklist        as X
