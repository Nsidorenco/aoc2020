{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import FileHelper

sampleInput =
  [ 35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ] ::
    [Int]

preambleSum :: [Int] -> Int -> Int -> Bool
preambleSum input n i =
  input !! i `elem` pre
  where
    pre = let lst = take n (drop (i - n) input) in [a + b | a <- lst, b <- lst, a /= b]

part1 :: [Int] -> Int -> Int
part1 input n = input !! head (dropWhile (preambleSum input n) [n .. length input])

part2Aux :: [Int] -> Int -> Int -> Int -> Int -> (Int, Int)
part2Aux input target acc i j
  | acc == target = (i, j)
  | target < acc = part2Aux input target (acc - (input !! i)) (i + 1) j
  | otherwise = part2Aux input target (acc + (input !! (j + 1))) i (j + 1)

part2 :: [Int] -> Int -> Int
part2 input n =
  let (i, j) = part2Aux input target (head input) 0 0
   in let lst = take (j - i + 1) (drop i input)
       in maximum lst + minimum lst
  where
    target = part1 input n

solve :: IO ()
solve = do
  file <- readInput 9
  let input = map (read . T.unpack) $ T.splitOn "\n" file
  print $ part1 input 25
  print $ part2 input 25
