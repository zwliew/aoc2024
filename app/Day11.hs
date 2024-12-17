module Day11 where

import Data.IntMap.Strict (IntMap, empty, foldrWithKey, insertWith)
import Utils (frequencies, getInput, numDigits)

parseInput :: String -> IntMap Int
parseInput = frequencies . map read . words

blink :: IntMap Int -> IntMap Int
blink = foldrWithKey f empty
  where
    f 0 c = insertWith (+) 1 c
    f x c
        | even digits = insertWith (+) q c . insertWith (+) r c
        | otherwise = insertWith (+) (x * 2024) c
      where
        digits = numDigits x
        halvedDigits = digits `div` 2
        (q, r) = x `divMod` (10 ^ halvedDigits)

solve1 :: IO String
solve1 = do
    xs <- getInput 11 parseInput
    let xs' = iterate blink xs !! 25
    return . show . sum $ xs'

solve2 :: IO String
solve2 = do
    counts <- getInput 11 parseInput
    let counts' = iterate blink counts !! 75
    return . show . sum $ counts'
