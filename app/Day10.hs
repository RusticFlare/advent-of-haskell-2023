module Day10 where

import Text.ParserCombinators.Parsec ( many )
import Parser ( parseText, integer )

day10 :: IO ()
day10 = do
   input <- readFile "inputs/Day10.txt"
   let integers = parseIntegers input
   print $ "Part 1: " ++ show (part1 integers)
   print $ "Part 2: " ++ show (part2 integers)

part1 :: a -> Integer
part1 _ = 0

part2 :: a -> Integer
part2 _ = 0

-- Types

-- Soloutions

-- Parser

parseIntegers :: String -> [Integer]
parseIntegers = parseText (many integer)
