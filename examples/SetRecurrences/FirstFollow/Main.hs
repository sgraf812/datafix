{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

import           System.Environment
import           Text.Printf
import           SetRecurrences.FirstFollow

main :: IO ()
main = do
  (k:_) <- map read <$> getArgs
  let uncurry3 f (x,y,z) = f x y z
  let analyse name gr (s :: Char) = do
        printf "%s:\n" name
        printf "  first_%d(%s): %s\n" k (show s) (show $ first k gr [NT s])
        printf "  follow_%d(%s): %s\n" k (show s) (show $ follow k gr s)
        putStrLn ""
  mapM_ (uncurry3 analyse) $
    [ ("Dyck", dyck, 'S')
    , ("LL(1), not SLL(k)", llsll, 'A')
    , ("empty", emptyL, 'A')
    , ("left recursive", leftrec, 'S')
    ]

