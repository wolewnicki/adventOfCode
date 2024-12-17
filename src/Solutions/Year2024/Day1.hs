module Solutions.Year2024.Day1 
  ( problem1
  , problem2
  ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Text.Read as Text


transposeInput :: String -> [[String]]
transposeInput input = List.transpose $ map words $ lines input

readInt :: String -> Int
readInt = Maybe.fromMaybe 0 . Text.readMaybe

parseInput :: [[String]] -> [[Int]]
parseInput = (map . map) readInt

zipLists :: [[a]] -> [(a, a)]
zipLists (xs:ys:_) = zip xs ys
zipLists [_]         = []
zipLists []          = []

calculateSimularityScore :: [[Int]] -> Int
calculateSimularityScore (xs:ys:_) = sum similarityScores
  where
    uniqueXs = List.nub xs
    groupedYs = List.group $ filter (`elem` uniqueXs) ys
    similarityScores = map (\gs -> head gs * length gs) groupedYs
calculateSimularityScore [_]       = 0
calculateSimularityScore []        = 0

findDistance :: (Int, Int) -> Int
findDistance (a,b) = max a b - min a b

problem1 :: String -> Int
problem1 =
  sum . map findDistance . zipLists . map List.sort . parseInput . transposeInput

problem2 :: String -> Int
problem2 = calculateSimularityScore . parseInput . transposeInput
