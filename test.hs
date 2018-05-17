{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}

module Foo where

class C a where
   getInt :: Int

instance C Char where
   getInt = 42

f :: (forall a. C a => Int) -> Bool
f x = even (x @ Char)

g :: (forall a. C a => Int) -> Bool
g = f                  -- fails
-- g h = f h           -- fails
-- g h = f getInt      -- fails
-- g _ = f 42          -- OK