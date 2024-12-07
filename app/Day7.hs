module Day7 where

import Data.Bifunctor (bimap)
import Utils (dropLast, getInput, numDigits)

parseInput :: String -> [(Int, [Int])]
parseInput = map (bimap (read . dropLast 1) (map read) . (\ss -> (head ss, tail ss)) . words) . lines

validate :: [Int -> Int -> Int] -> Int -> Int -> [Int] -> Bool
validate _ t n [] = t == n
validate fs t n (x : xs) = any (\f -> validate fs t (f n x) xs) fs

checkEqns :: [Int -> Int -> Int] -> [(Int, [Int])] -> Int
checkEqns fs = sum . map fst . filter (\(t, xs) -> validate fs t (head xs) (tail xs))

solve1 :: IO String
solve1 = do
    eqns <- getInput 7 parseInput
    return . show $ checkEqns [(+), (*)] eqns

solve2 :: IO String
solve2 = do
    eqns <- getInput 7 parseInput
    return . show $ checkEqns [(+), (*), \t n -> t * 10 ^ numDigits n + n] eqns
