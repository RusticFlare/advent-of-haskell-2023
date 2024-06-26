module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)

main :: IO ()
main = do
    putStrLn "Day?"
    runDay =<< getLine

runDay :: String -> IO ()
runDay day = case read day :: Int of
    1  -> day01
    2  -> day02
    3  -> day03
    4  -> day04
    5  -> day05
    6  -> day06
    7  -> day07
    8  -> day08
    9  -> day09
    10 -> day10
    _ -> error $ day ++ " is not a valid day"
