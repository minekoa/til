module ExH where

import Prelude hiding (take, drop, splitAt)


take' :: Int -> [a] -> [a]
take' n xxs
  | n <= 0    = []
  | otherwise = case xxs of
      x : xs -> x : take' (n-1) xs
      []     -> []


drop' :: Int -> [a] -> [a]
drop' n xxs
  | n <= 0    = xxs
  | otherwise = case xxs of
    x : xs -> drop' (n - 1 ) xs
    []     -> []

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xxs
  | n <= 0    = ([], xxs)
  | otherwise = case xxs of
      x : xs -> (x : hd, tl) where (hd,tl) = splitAt' (n-1) xs
      []     -> ([], [])

