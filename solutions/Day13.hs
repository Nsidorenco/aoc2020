{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Control.Monad (zipWithM)
import Data.Foldable (minimumBy)
import Data.List (foldl')
import Data.Ord (comparing)
import qualified Data.Text as T
import FileHelper

sampleInput = "939\n7,13,x,x,59,x,31,19\n" :: T.Text

sampleInput1 = "0\n17,x,13,19" :: T.Text

sampleInput2 = "0\n67,7,59,61" :: T.Text

sampleInput3 = "0\n67,x,7,59,61" :: T.Text

sampleInput4 = "0\n67,7,x,59,61" :: T.Text

sampleInput5 = "0\n1789,37,47,1889" :: T.Text

parseInput :: [T.Text] -> (Int, [Int])
parseInput (x : xs) = (read (T.unpack x), rest)
  where
    rest = map (read . T.unpack) $ filter (/= "x") $ concatMap (T.splitOn ",") xs

part1 :: [T.Text] -> Int
part1 txt =
  let (target, busses) = parseInput txt
   in let timetable = map (\x -> (((target `quot` x) + 1) * x) - target) busses
       in let (time, id) = minimumBy (comparing fst) (zip timetable busses) in time * id

parseInputWithOrder :: [T.Text] -> [(Integer, Integer)]
parseInputWithOrder (_ : xs) =
  map (\(x, i) -> (- i, read $ T.unpack x)) $ filter (\(x, _) -> x /= "x") $ zip (concatMap (T.splitOn ",") xs) [0 ..]

crt :: [(Integer, Integer)] -> Integer
crt eqs = foldr (\(a, m) acc -> acc + (a * b m * (b m `invMod` m))) 0 eqs `mod` terms
  where
    terms = product $ map snd eqs
    b m = terms `div` m
    -- Modular Inverse
    a `invMod` m = let (_, i, _) = gcd a m in i `mod` m
    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd (b `mod` a) a

part2 :: [T.Text] -> Integer
part2 txt = crt busses
  where
    busses = parseInputWithOrder txt

solve :: IO ()
solve = do
  file <- readInput 13
  -- let file = T.strip sampleInput1
  let input = T.splitOn "\n" file
  print $ parseInputWithOrder input
  print $ part2 input
