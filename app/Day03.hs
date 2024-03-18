{-# LANGUAGE TupleSections #-}

module Day03 where

import Text.Regex.TDFA
import qualified Data.Set as Set
import Data.Foldable.Extra (sumOn')

day03 :: IO ()
day03 = do
   input <- readFile "inputs/Day03.txt"
   print $ "Part 1: " ++ show (day03Part1 input)
   print $ "Part 2: " ++ show (day03Part2 input)

type Line = Int
type Column = Int

-- Set of locations to symbols
-- Filter numbers if they are next to a symbol
-- Sum the numbers
day03Part1 :: String -> Int
day03Part1 input =
    let symbols = symbolLocations input
        numbers = nums input
    in sumOn' (score symbols) numbers

day03Part2 :: String -> Integer
day03Part2 _ = 1

symbolLocations :: String -> Set.Set (Line, Column)
symbolLocations input = Set.fromList $ concat $ zipWith foo [0..] (lines input)

foo :: Line -> String -> [(Line, Column)]
foo i s = map (i,) (allSymbolOffsets s)

nums :: String -> [(Line, (Int, (Column, MatchLength)))]
nums input = concat $ zipWith bar [0..] (lines input)

bar :: Line -> String -> [(Line, (Int, (Column, MatchLength)))]
bar i s = map (i,) (allNumbers s)

score :: Set.Set (Line, Column) -> (Line, (Int, (Column, MatchLength))) -> Int
score s (l, (v, (c, m)))
    | valid s (l, (v, (c, m))) = v
    | otherwise = 0

valid :: Set.Set (Line, Column) -> (Line, (Int, (Column, MatchLength))) -> Bool
valid s n = any (`Set.member` s) (points n)

points :: (Line, (Int, (Column, MatchLength))) -> [(Line, Column)]
points (l, (_, (c, m))) = concatMap (\l' -> map (l',) [c-1..c+m]) [l-1..l+1]

-- Regex

allNumbers :: String -> [(Int, (Column, MatchLength))]
allNumbers line  = zip (allIntMatches line) (allIntMatchLocations line)

allIntMatches :: String -> [Int]
allIntMatches line = map read (allIntTextMatches line)

allIntTextMatches :: String -> [String]
allIntTextMatches line = getAllTextMatches (line =~ "[0-9]+")

allIntMatchLocations :: String -> [(Column, MatchLength)]
allIntMatchLocations line = getAllMatches (line =~ "[0-9]+")

allSymbolOffsets :: String -> [Column]
allSymbolOffsets line = map fst (getAllMatches (line =~ "[^0-9\\.]") :: [(MatchOffset, MatchLength)])
