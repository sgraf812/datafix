{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Datafix
-- Copyright   :  (c) Sebastian Graf 2017-2020
-- License     :  ISC
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- This is the top-level, import-all, kitchen sink module.
--
-- Look at "Datafix.Tutorial" for a tour guided by use cases.

module Datafix
  ( module Datafix.Common
  , module Datafix.Denotational
  , module Datafix.Explicit
  , module Datafix.NodeAllocator
  , module Datafix.FrameworkBuilder
  , Datafix.MonoMap.MonoMap
  , module Datafix.Utils.TypeLevel
  , module Datafix.Worklist
  ) where

import           Datafix.Common
import           Datafix.Denotational
import           Datafix.Explicit
import           Datafix.MonoMap
import           Datafix.NodeAllocator
import           Datafix.FrameworkBuilder
import           Datafix.Utils.TypeLevel hiding (Map)
import           Datafix.Worklist
