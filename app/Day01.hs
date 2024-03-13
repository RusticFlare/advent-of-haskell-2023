module Day01 where

import Data.Char (isDigit, digitToInt)

day01 :: IO ()
day01 = do
   input <- readFile "inputs/Day01.txt"
   print $ day01Part1 input
   print $ day01Part2 input

day01Part1 :: String -> Int
day01Part1 input = sum $ map calibrationValue1 $ words input

day01Part2 :: String -> Int
day01Part2 input = sum $ map calibrationValue2 $ words input

calibrationValue1 :: String -> Int
calibrationValue1 line = read [head digits, last digits]
    where digits = filter isDigit line

calibrationValue2 :: String -> Int
calibrationValue2 line = (head digits * 10) + last digits
    where digits = allDigits line

allDigits :: String -> [Int]
allDigits [] = []
allDigits ('o':'n':'e':xs) = 1 : allDigits ('e':xs)
allDigits ('t':'w':'o':xs) = 2 : allDigits ('o':xs)
allDigits ('t':'h':'r':'e':'e':xs) = 3 : allDigits ('e':xs)
allDigits ('f':'o':'u':'r':xs) = 4 : allDigits xs
allDigits ('f':'i':'v':'e':xs) = 5 : allDigits ('e':xs)
allDigits ('s':'i':'x':xs) = 6 : allDigits xs
allDigits ('s':'e':'v':'e':'n':xs) = 7 : allDigits ('n':xs)
allDigits ('e':'i':'g':'h':'t':xs) = 8 : allDigits ('t':xs)
allDigits ('n':'i':'n':'e':xs) = 9 : allDigits ('e':xs)
allDigits (x:xs)
    | isDigit x = digitToInt x : allDigits xs
    | otherwise = allDigits xs
