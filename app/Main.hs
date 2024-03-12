module Main where

import Data.Char (isDigit)

main :: IO ()
main = do
   input <- readFile "inputs/Day01.txt"
   print $ day01Part1 input

day01Part1 :: String -> Int
day01Part1 input = sum $ map calibrationValue $ words input

calibrationValue :: String -> Int
calibrationValue line = read [head digits, last digits]
    where digits = filter isDigit line
