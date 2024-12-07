module Day4 where

import Data.Array (Array, bounds, inRange, indices, listArray, (!), (//))
import Utils (getInput)

type Index = (Int, Int)

parseInput :: String -> Array Index Char
parseInput s = listArray ((0, 0), (height ss - 1, width ss - 1)) . concat . lines $ s
  where
    ss = lines s
    width = length . head
    height = length

walkArray :: (Array Index Char -> Index -> Int) -> Array Index Char -> Index -> Int
walkArray f arr (row, col)
    | row > w1 = 0
    | col > h1 = walkArray f arr (row + 1, h0)
    | otherwise = walkArray f arr (row, col + 1) + f arr (row, col)
  where
    ((_, h0), (w1, h1)) = bounds arr

matches :: Array Index Char -> [(Char, Index)] -> Bool
matches arr = all (\(c, i) -> inRange (bounds arr) i && arr ! i == c)

countMatches :: Array Index Char -> [[(Char, Index)]] -> Int
countMatches arr = length . filter id . map (matches arr)

solve1 :: IO String
solve1 = do
    arr <- getInput 4 parseInput
    let (si, (h1, w1)) = bounds arr
    let flipped = arr // [((row, col), arr ! (h1 - row, w1 - col)) | (row, col) <- indices arr]
    let checkAndCount arr' (row, col) = countMatches arr' $ map (zip "XMAS") [[(row, col + i) | i <- [0 .. 3]], [(row + i, col) | i <- [0 .. 3]], [(row + i, col + i) | i <- [0 .. 3]], [(row + i, col - i) | i <- [0 .. 3]]]
    let numXmas = walkArray checkAndCount arr si + walkArray checkAndCount flipped si
    return $ show numXmas

solve2 :: IO String
solve2 = do
    arr <- getInput 4 parseInput
    let (si, _) = bounds arr
    let checkAndCount arr' (row, col) = countMatches arr' $ [zip cs | cs <- ["AMMSS", "ASSMM", "AMSMS", "ASMSM"]] <*> pure [(row + 1, col + 1), (row, col), (row, col + 2), (row + 2, col), (row + 2, col + 2)]
    let numXmas = walkArray checkAndCount arr si
    return $ show numXmas
