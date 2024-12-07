module Day5 where

import Data.Bifunctor (bimap)
import Data.List (sortBy)
import Utils (getInput, middle, splitOn)

parseInput :: String -> ([[Int]], [[Int]])
parseInput = bimap (map (map read . splitOn '|')) (map (map read . splitOn ',') . tail) . break null . lines

validate :: [[Int]] -> [Int] -> Bool
validate = aux []
  where
    aux :: [Int] -> [[Int]] -> [Int] -> Bool
    aux _ _ [] = True
    aux ys rs (x : xs) = all (\y -> [y, x] `elem` rs) ys && aux (x : ys) rs xs

validateAll :: [[Int]] -> [[Int]] -> [[Int]]
validateAll rs = filter (validate rs)

sumMiddles :: [[Int]] -> Int
sumMiddles = sum . map middle

solve1 :: IO String
solve1 = do
    (rs, xs) <- getInput 5 parseInput
    return . show . sumMiddles . validateAll rs $ xs

validateAll2 :: [[Int]] -> [[Int]] -> [[Int]]
validateAll2 rs = map (sortBy cmp) . filter (not . validate rs)
  where
    cmp :: Int -> Int -> Ordering
    cmp x y
        | [x, y] `elem` rs = LT
        | [y, x] `elem` rs = GT
        | otherwise = EQ

solve2 :: IO String
solve2 = do
    (rs, xs) <- getInput 5 parseInput
    return . show . sumMiddles . validateAll2 rs $ xs
