{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.List (nub)
import qualified Data.Text as T
import FileHelper

findCombinations :: Int -> Int -> [Int] -> Int
findCombinations n val input =
  head $ [product xs | xs <- mapM (const input) [1 .. n], sum (nub xs) == val]

part1 :: [Int] -> Int
part1 = findCombinations 2 2020

part2 :: [Int] -> Int
part2 = findCombinations 3 2020

-- part1 : 927684
-- part2 : 292093004
solve :: IO ()
solve = do
  file <- readInput 1
  let input = read . T.unpack <$> T.splitOn "\n" file :: [Int]
  print $ part1 input
  print $ part2 input
