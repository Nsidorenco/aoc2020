{-# LANGUAGE BangPatterns #-}

module Day23 where

import Data.Foldable (Foldable (foldl'))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List ((\\))
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

sampleInput = [3, 8, 9, 1, 2, 5, 4, 6, 7] :: [Int]

step :: Int -> IntMap Int -> Int -> (Int, IntMap Int)
step max st cur = let (!newCur, !newState) = (cur', st') in (newCur, newState)
  where
    c1 = fromJust $ M.lookup cur st
    c2 = fromJust $ M.lookup c1 st
    c3 = fromJust $ M.lookup c2 st
    cur' = fromJust $ M.lookup c3 st
    dest =
      let options = [cur, cur - 1 .. 1] ++ [max, max -1 .. 1]
       in head $ options \\ [cur, c1, c2, c3]
    dest' = fromJust $ M.lookup dest st
    st' = M.insert cur cur' $ M.insert c3 dest' $ M.insert dest c1 st

run :: Int -> IntMap Int -> Int -> Int -> (Int, IntMap Int)
run max st e n =
  foldl' (\(!i, !st') _ -> step max st' i) (e, st) [1 .. n]

constructMap :: [Int] -> IntMap Int
constructMap input = st
  where
    map = foldl' (\map (e, e') -> M.insert e e' map) M.empty (zip input (tail input))
    st = M.insert (last input) (head input) map

part1 :: [Int] -> [Int]
part1 input = scanl (\e _ -> fromJust $ M.lookup e st') 1 [1 .. (max -1)]
  where
    max = maximum input
    st = constructMap input
    st' = snd $ run max st (head input) 100

part2 :: [Int] -> Int
part2 input = c1 * c2
  where
    n = 1000000
    max = maximum input
    input' = input ++ [max + 1 .. n]
    st = constructMap input'
    st' = snd $ run n st (head input') 10000000
    c1 = fromJust $ M.lookup 1 st'
    c2 = fromJust $ M.lookup c1 st'

solve = do
  print $ part1 puzzle
  print $ part2 puzzle

puzzle = [3, 6, 4, 2, 9, 7, 5, 8, 1] :: [Int]
