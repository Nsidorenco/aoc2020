{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Control.Monad.ST (runST)
import Data.Foldable (Foldable (foldr'))
import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Linear as HL
import qualified Data.IntMap.Strict as M

sampleInput = [0, 3, 6] :: [Int]

input = [6, 19, 0, 5, 7, 13, 1] :: [Int]

-- This is not really any faster than the immutable solution..
mutableSolution :: [Int] -> Int -> Int
mutableSolution xs target = runST $ do
  mem <- HT.fromList $ zip (init xs) [1 ..]
  recur mem (last xs) (length xs)
  where
    recur mem last i
      | target <= i = pure last
      | otherwise = do
        val <- HL.lookup mem last
        case val of
          Just idx -> do
            HL.insert mem last i
            recur mem (i - idx) (i + 1)
          Nothing -> do
            HL.insert mem last i
            recur mem 0 (i + 1)

initialState :: [Int] -> M.IntMap Int
initialState start = M.fromList $ zip (init start) [1 ..]

nextState :: (Int, M.IntMap Int, Int) -> (Int, M.IntMap Int, Int)
nextState (last, state, i) =
  if M.member last state
    then (i - state M.! last, newState, i + 1)
    else (0, newState, i + 1)
  where
    newState = M.insert last i state

nthState :: Int -> [Int] -> (Int, M.IntMap Int, Int)
nthState n start =
  foldr' -- Need strict fold such that functions are composed and not trunked at composed at call-time
    ($!) -- NOTE: $! forces strictness of the proceeding arguments. Will stack overflow if not provided.
    -- This is due to extreme trunk build-up. Since function evaluations are queued, but not evaluated.
    (last start, initialState start, length start)
    (replicate (n - length start) nextState)

part1 :: [Int] -> Int
part1 = flip mutableSolution 2020

part2 :: [Int] -> Int
part2 = flip mutableSolution 30000000

solve :: IO ()
solve = do
  print $ part1 input
  print $ part2 input
