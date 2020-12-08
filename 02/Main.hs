module Main where

import System.Environment
import Text.Printf

data Row = Row { rowChar :: Char
               , rowRange :: (Int, Int) 
               , rowPassword :: String
               } deriving Show

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d s = x:splitOn d (tail' s')
  where (x, s') = span (/= d) s

sample :: String
sample = "1-3 a: abcde"

rowFromString :: String -> Row
rowFromString s = Row c (read low, read high) password
  where [policy, ' ':password] = splitOn ':' s
        [range, c:_] = splitOn ' ' policy
        [low, high] = splitOn '-' range

isCorrectV1 :: Row -> Bool
isCorrectV1 (Row c (low, high) password) = low <= n && n <= high
  where n = length $ filter (== c) password

isCorrectV2 :: Row -> Bool
isCorrectV2 (Row c (low, high) password) = (a == c) /= (b == c)
  where a = password !! (low - 1)
        b = password !! (high - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      printf "Input file: %s\n" filePath
      rows <- map rowFromString . lines <$> readFile filePath
      printf "Part 1: %d\n" $ length $ filter isCorrectV1 rows 
      printf "Part 2: %d\n" $ length $ filter isCorrectV2 rows
    _ -> error "Input file is not provided"
