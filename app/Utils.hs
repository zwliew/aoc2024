module Utils where

import Data.List (sort)
import Data.Maybe (fromMaybe)

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

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

last2 :: [a] -> Maybe (a, a)
last2 [] = Nothing
last2 [_] = Nothing
last2 [x, y] = Just (x, y)
last2 (_ : xs) = last2 xs

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

numDigits :: Int -> Int
numDigits = length . show

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = ys : splitOn c rest
  where
    (ys, zs) = break (== c) xs
    rest = fromMaybe [] . tailMay $ zs

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_ : xs) = Just xs

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

getInput :: Int -> (String -> a) -> IO a
getInput n parse = parse <$> readFile ("inputs/day" ++ show n ++ ".txt")
