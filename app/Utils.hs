module Utils where

import Data.List (sort)

counts :: [Int] -> [(Int, Int)]
counts = map (\xs -> (head xs, length xs)) . group . sort

group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x : xs) = (x : ys) : group zs
  where
    (ys, zs) = span (== x) xs

isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy _ [] = True
isSortedBy _ [_] = True
isSortedBy f (x : x' : xs) = f x x' && isSortedBy f (x' : xs)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : x' : xs) = (x, x') : pairs (x' : xs)
