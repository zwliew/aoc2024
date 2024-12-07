module Main where

import Day3 (solve1, solve2)

main :: IO ()
main = solve1 >>= putStrLn >> solve2 >>= putStrLn
