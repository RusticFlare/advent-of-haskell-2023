module Day09 where

import Text.ParserCombinators.Parsec ( many )
import Parser ( parseText, integer )
import Data.Foldable.Extra (sumOn')

day09 :: IO ()
day09 = do
   input <- readFile "inputs/Day09.txt"
   let documents = map parseValueHistory $ lines input
   print $ "Part 1: " ++ show (part1 documents)
   print $ "Part 2: " ++ show (part2 documents)

part1 :: [[Integer]] -> Integer
part1 = sumOn' nextValue

part2 :: [[Integer]] -> Integer
part2 = sumOn' previousValue

-- Soloutions

nextValue :: [Integer] -> Integer
nextValue vs
    | all (0==) vs = 0
    | otherwise = last vs + nextValue (differences vs)

previousValue :: [Integer] -> Integer
previousValue vs
    | all (0==) vs = 0
    | otherwise = head vs - previousValue (differences vs)

differences :: [Integer] -> [Integer]
differences = mapAdjacent (flip (-))

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f (x1:x2:xs) = f x1 x2 : mapAdjacent f (x2:xs)
mapAdjacent _ _ = []

-- Parser

parseValueHistory :: String -> [Integer]
parseValueHistory = parseText (many integer)
