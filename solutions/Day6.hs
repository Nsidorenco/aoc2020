{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Bifunctor (Bifunctor (first))
import Data.List (nub)
import Data.Map (fromListWith, toList)
import qualified Data.Text as T
import FileHelper

sampleInput =
  [ "abc",
    "",
    "a",
    "b",
    "c",
    "",
    "ab",
    "ac",
    "",
    "a",
    "a",
    "a",
    "a",
    "",
    "b"
  ]

part1 :: [String] -> Int
part1 = sum . map (length . nub)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

part2 :: [(String, Int)] -> Int
part2 =
  length
    . concatMap
      ((\(lst, n) -> filter (\(_, i) -> i == n) lst) . first frequency)

solve :: IO ()
solve = do
  file <- readInput 6
  let input1 = T.unpack <$> T.splitOn " " (T.replace "\n" "" $ T.replace "\n\n" " " file)
  let votes = T.splitOn "  " (T.replace "\n" " " $ T.replace "\n\n" "  " file)
  let ns = (+ 1) . T.count " " <$> votes
  let input2 = zip (T.unpack . T.replace " " "" <$> votes) ns
  print $ part1 input1
  print $ part2 input2
