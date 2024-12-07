module Day3 where

import Control.Applicative
import Text.Parsec (anyChar, digit, many1, parse, string, try)
import Text.Parsec.String (Parser)

data Match = Do | Dont | Mul (Int, Int) | Skipped
    deriving (Show, Eq)

parseDo :: Parser Match
parseDo = Do <$ string "do()"

parseDont :: Parser Match
parseDont = Dont <$ string "don't()"

parseMul :: Parser Match
parseMul = do
    _ <- string "mul("
    x <- many1 digit
    _ <- string ","
    y <- many1 digit
    _ <- string ")"
    return $ Mul (read x, read y)

skipOneChar :: Parser Match
skipOneChar = anyChar >> return Skipped

parsePattern :: Parser Match
parsePattern = try parseDo <|> try parseDont <|> try parseMul <|> skipOneChar

parseInput :: String -> Parser a -> a
parseInput input parser = case parse parser "" input of
    Left err -> error $ show err
    Right res -> res

walkTokens :: (Bool -> Bool) -> (Bool, Int) -> Match -> (Bool, Int)
walkTokens _ (b, n) Skipped = (b, n)
walkTokens _ (_, n) Do = (True, n)
walkTokens _ (_, n) Dont = (False, n)
walkTokens f (b, n) (Mul (x, y)) = (b, n + if f b then x * y else 0)

getInput :: IO [Match]
getInput = do
    input <- readFile "inputs/day3.txt"
    return $ parseInput input (many1 parsePattern)

solve1 :: IO String
solve1 = do
    matches <- getInput
    let multSum = snd $ foldl (walkTokens (const True)) (True, 0) $ filter (/= Skipped) matches
    return $ show multSum

solve2 :: IO String
solve2 = do
    matches <- getInput
    let multSum = snd $ foldl (walkTokens id) (True, 0) $ filter (/= Skipped) matches
    return $ show multSum
