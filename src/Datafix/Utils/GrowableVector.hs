module Datafix.Utils.GrowableVector
  ( GrowableVector
  , new
  , length
  , pushBack
  , write
  , freeze
  ) where

import           Control.Monad.Primitive
import           Data.Primitive.Array
import           Data.Primitive.MutVar
import           Prelude                 hiding (length)

data GrowableVector s v
  = GrowableVector
  { buffer :: !(MutableArray s v)
  , len    :: !Int
  }

notInitializedError :: a
notInitializedError = error "newGrowableVector: Accessed uninitialized value"

new :: PrimMonad m => Int -> m (GrowableVector (PrimState m) v)
new c =
  GrowableVector <$> newArray c notInitializedError <*> pure 0
{-# INLINE new #-}

capacity :: GrowableVector s v -> Int
capacity = sizeofMutableArray . buffer
{-# INLINE capacity #-}

length :: GrowableVector s v -> Int
length = len
{-# INLINE length #-}

grow :: PrimMonad m => GrowableVector (PrimState m) v -> Int -> m (GrowableVector (PrimState m) v)
grow vec n = do
  arr <- newArray (capacity vec + n) notInitializedError
  copyMutableArray arr 0 (buffer vec) 0 (len vec)
  return (GrowableVector arr (len vec))
{-# INLINE grow #-}

pushBack :: PrimMonad m => GrowableVector (PrimState m) v -> v -> m (GrowableVector (PrimState m) v)
pushBack vec v = do
  vec' <- if length vec == capacity vec
    then grow vec (max 1 (capacity vec))
    else return vec
  writeArray (buffer vec') (len vec') v
  return vec' { len = len vec' + 1 }
{-# INLINE pushBack #-}

write :: PrimMonad m => GrowableVector (PrimState m) v -> Int -> v -> m ()
write = writeArray . buffer
{-# INLINE write #-}

freeze :: PrimMonad m => GrowableVector (PrimState m) v -> m (Array v)
freeze vec = freezeArray (buffer vec) 0 (len vec)
{-# INLINE freeze #-}
