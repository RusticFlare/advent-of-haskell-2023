module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)

main :: IO ()
main = do
    putStrLn "Day?"
    runDay =<< getLine

runDay :: String -> IO ()
runDay day = case read day :: Int of
    1 -> day01
    2 -> day02
    3 -> day03
    4 -> day04
    _ -> error $ day ++ " is not a valid day"
