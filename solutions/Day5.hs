{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Control.Arrow (Arrow (first))
import Data.Char
import Data.List (sort)
import qualified Data.Text as T
import FileHelper
import GHC.List (foldl')

sampleInput =
  [ "BFFFBBFRRR",
    "FFFBBBFRRR",
    "BBFFBBFRLL"
  ]

higherEnd (low, high) = (low, (high - low) `div` 2 + low)

lowerEnd (low, high) = (low + ((high - low) `div` 2 + 1), high)

findRow :: String -> Int
findRow s =
  fst $
    foldl'
      (\t x -> if x == 'F' then higherEnd t else lowerEnd t)
      (0, 127)
      s

findColumn :: String -> Int
findColumn s =
  snd $
    foldl'
      (\t x -> if x == 'L' then higherEnd t else lowerEnd t)
      (0, 7)
      s

splitInput :: String -> (String, String)
splitInput = span (`elem` ['F', 'B'])

findSeat :: String -> (Int, Int)
findSeat s =
  let (row, col) = splitInput s
   in (findRow row, findColumn col)

part1 :: [String] -> Int
part1 s = maximum $ map ((\(row, col) -> row * 8 + col) . findSeat) s

part2 :: [String] -> Int
part2 s =
  let seats = sort (map ((\(row, col) -> row * 8 + col) . findSeat) s)
   in (+ 1) (fst (head (filter (\(n, m) -> n + 1 /= m) $ zip seats (tail seats))))

solve :: IO ()
solve = do
  file <- readInput 5
  let input = T.unpack <$> T.splitOn "\n" file
  print $ part1 input
  print $ part2 input

-- print $ (map (\x -> validateEntryFields x && validateEntry x)) $ parseEntries input
