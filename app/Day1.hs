module Day1 where

import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe)
import Utils (counts, getInput, last2)

parseInput :: String -> ([Int], [Int])
parseInput = unzip . map (fromJust . last2 . parseLine) . lines
  where
    parseLine :: String -> [Int]
    parseLine "" = []
    parseLine s = case reads s :: [(Int, String)] of
        [(n, s')] -> n : parseLine s'
        [] -> []
        _ -> error "parseLine: ambiguous parse"

solve1 :: IO String
solve1 = do
    (xs, ys) <- getInput 1 parseInput
    let sorted = zip (sort xs) (sort ys)
    let diffs = map (\(x, y) -> abs (y - x)) sorted
    let diffSum = sum diffs
    return $ show diffSum

solve2 :: IO String
solve2 = do
    (xs, ys) <- getInput 1 parseInput
    let ys' = counts ys
    let scores = map (\x -> x * fromMaybe 0 (lookup x ys')) xs
    let scoreSum = sum scores
    return $ show scoreSum
