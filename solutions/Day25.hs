{-# LANGUAGE OverloadedStrings #-}

module Day24 where

import Data.List (find)
import Data.Maybe (fromJust)
import GHC.List (scanl')

cardPKSample = 5764801 :: Int

doorPKSample = 17807724 :: Int

cardPK = 14222596 :: Int

doorPK = 4057428 :: Int

enc :: Int -> Int -> Int
enc key x = foldr (\_ acc -> acc * x `mod` 20201227) 1 [1 .. key]

-- | Solves enc 7 i == pk
computeSK :: Int -> Int
computeSK pk = fst . fromJust . find (\(_, pk') -> pk' == pk) $ scanl' (\(_, acc) i -> (i, acc * 7 `mod` 20201227)) (0, 1) [1 ..]

part1 :: Int -> Int -> Int
part1 cardPK doorPK = enc sk doorPK
  where
    sk = computeSK cardPK

-- solve = do
--   -- fileRaw <- T.readFile $ "inputs/day20_test.txt"
--   file <- readInput 24
--   let input = T.splitOn "\n" file
--   print $ part1 input
--   print $ part2 input
