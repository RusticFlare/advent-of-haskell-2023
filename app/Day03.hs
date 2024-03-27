{-# LANGUAGE TupleSections #-}

module Day03 where

import Text.Regex.TDFA
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable.Extra (sumOn')
import Data.List (nub)
import Data.Maybe (mapMaybe)

day03 :: IO ()
day03 = do
   input <- readFile "inputs/Day03.txt"
   print $ "Part 1: " ++ show (day03Part1 input)
   print $ "Part 2: " ++ show (day03Part2 input)

type Line = Int
type Column = Int

day03Part1 :: String -> Int
day03Part1 input =
    let symbols = symbolLocations input
        numbers = nums input
    in sumOn' (score symbols) numbers

day03Part2 :: String -> Int
day03Part2 input =
    let gears = gearLocations input
        numbers = numsByCoords $ nums input
    in sumOn' (gearScore numbers) gears

symbolLocations :: String -> Set.Set (Line, Column)
symbolLocations input = Set.fromList $ concat $ zipWith symbolsOnLine [0..] (lines input)

symbolsOnLine :: Line -> String -> [(Line, Column)]
symbolsOnLine i s = map (i,) (allSymbolOffsets s)

gearLocations :: String -> [(Line, Column)]
gearLocations input = concat $ zipWith symbolsOnLine [0..] (lines input)

gearsOnLine :: Line -> String -> [(Line, Column)]
gearsOnLine i s = map (i,) (allGearOffsets s)

nums :: String -> [(Line, (Int, (Column, MatchLength)))]
nums input = concat $ zipWith numsOnLine [0..] (lines input)

numsOnLine :: Line -> String -> [(Line, (Int, (Column, MatchLength)))]
numsOnLine i s = map (i,) (allNumbers s)

numsByCoords :: [(Line, (Int, (Column, MatchLength)))] -> Map.Map (Line, Column) (Line, (Int, (Column, MatchLength)))
numsByCoords ns = Map.fromList $ concatMap coords ns

coords :: (Line, (Int, (Column, MatchLength))) -> [((Line, Column), (Line, (Int, (Column, MatchLength))))]
coords (l, (v, (c, m))) = map (\c' -> ((l, c'), (l, (v, (c, m))))) [c..c+m-1]

score :: Set.Set (Line, Column) -> (Line, (Int, (Column, MatchLength))) -> Int
score s (l, (v, (c, m)))
    | valid s (l, (v, (c, m))) = v
    | otherwise = 0

valid :: Set.Set (Line, Column) -> (Line, (Int, (Column, MatchLength))) -> Bool
valid s n = any (`Set.member` s) (points n)

points :: (Line, (Int, (Column, MatchLength))) -> [(Line, Column)]
points (l, (_, (c, m))) = concatMap (\l' -> map (l',) [c-1..c+m]) [l-1..l+1]

gearScore :: Map.Map (Line, Column) (Line, (Int, (Column, MatchLength))) -> (Line, Column) -> Int
gearScore m g = case numsAround m g of
             [x,y] -> x * y
             _ -> 0

numsAround :: Map.Map (Line, Column) (Line, (Int, (Column, MatchLength))) -> (Line, Column) -> [Int]
numsAround m g = map (\(_, (v, (_, _))) -> v) $ nub $ mapMaybe (`Map.lookup` m) (around g)

around :: (Line, Column) -> [(Line, Column)]
around (l, c) = concatMap (\l' -> map (l',) [c-1..c+1]) [l-1..l+1]

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

allGearOffsets :: String -> [Column]
allGearOffsets line = map fst (getAllMatches (line =~ "\\*") :: [(MatchOffset, MatchLength)])
