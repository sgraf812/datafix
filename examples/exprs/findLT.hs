module Expr where

expr :: Int -> [Int] -> Maybe Int
expr a b = findLT a b
  where
    findLT _ [] = Nothing
    findLT n (x:xs)
      | x < n = findLTJust n x xs
      | otherwise = findLT n xs
    findLTJust n m [] = Just m
    findLTJust n m (x:xs)
      | x < n && x > m = findLTJust n m xs
      | otherwise = findLTJust n x xs
