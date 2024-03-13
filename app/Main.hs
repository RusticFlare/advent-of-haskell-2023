module Main where

import Day01 (day01)

main :: IO ()
main = do
    putStrLn "Day?"
    day <- getLine
    runDay day

runDay :: String -> IO ()
runDay day = case read day :: Int of
    1 -> day01
    _ -> error $ day ++ " is not a valid day"
