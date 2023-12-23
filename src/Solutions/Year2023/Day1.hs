module Solutions.Year2023.Day1
  ( problem1
  , problem2
  ) where

import Data.Char (isNumber)
import Data.List (elemIndex)
import Control.Monad (guard)

stringToInt :: String -> Int
stringToInt s = read s :: Int

getFirstLast :: String -> String
getFirstLast s = [head s, last s]

firstLastToInt :: String -> Int
firstLastToInt = stringToInt . getFirstLast . filter isNumber

firstLastToIntPart2 :: String -> Int
firstLastToIntPart2 = stringToInt . getFirstLast . getNumberOrNumberWords

getNumberOrNumberWords :: String -> String
getNumberOrNumberWords s = concatMap numberWordToDigit $ extractNumbersAndNumberWords s

day1Solve :: (String -> Int) -> String -> Int
day1Solve firstLastFn input = sum $ map firstLastFn $ words input

problem1 :: String -> Int
problem1 = day1Solve firstLastToInt

problem2 :: String -> Int
problem2 = day1Solve firstLastToIntPart2

numberWords :: [String]
numberWords =
  [ "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

numbers :: [String]
numbers = map show [1..9 :: Integer]


numberWordToDigit :: String -> String
numberWordToDigit word = maybe word (show . (+ 1)) (elemIndex word numberWords)

extractNumbersAndNumberWords :: String -> [String]
extractNumbersAndNumberWords s = do
  start <- [0..(length s - 1)]
  len <- [1..(min 5 (length s - start))]

  let word = take len (drop start s)

  guard $ word `elem` numberWords || word `elem` numbers
  return word
