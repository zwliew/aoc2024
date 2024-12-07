module Day2 where

import Data.Maybe (fromJust, isNothing)
import Utils (getInput)

parseInput :: String -> [[Int]]
parseInput = map parseLine . lines
  where
    parseLine :: String -> [Int]
    parseLine "" = []
    parseLine s = case reads s :: [(Int, String)] of
        [(n, s')] -> n : parseLine s'
        [] -> []
        _ -> error "parseLine: ambiguous parse"

isSafe :: Int -> [Int] -> Bool
isSafe n xs = walkLevels ascendingPred n Nothing xs || walkLevels descendingPred n Nothing xs
  where
    ascendingPred x y = y - x >= 1 && y - x <= 3
    descendingPred = flip ascendingPred

    walkLevels :: (Int -> Int -> Bool) -> Int -> Maybe Int -> [Int] -> Bool
    walkLevels _ _ _ [] = True
    walkLevels _ _ _ [_] = True
    walkLevels f n' p (x : y : xs')
        | f x y = walkLevels f n' (Just x) (y : xs')
        | n' == 0 = False
        | otherwise = walkLevels f (n' - 1) p (x : xs') || ((isNothing p || f (fromJust p) y) && walkLevels f (n' - 1) p (y : xs'))

solve1 :: IO String
solve1 = do
    xs <- getInput 2 parseInput
    let trueCount = length $ filter id $ map (isSafe 0) xs
    return $ show trueCount

solve2 :: IO String
solve2 = do
    xs <- getInput 2 parseInput
    let trueCount = length $ filter id $ map (isSafe 1) xs
    return $ show trueCount
