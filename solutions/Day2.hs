{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import FileHelper

valid :: (Int, Int, Char, String) -> Bool
valid (low, high, c, s) =
  let n = length $ filter (== c) s
   in low <= n && n <= high

validPostition :: (Int, Int, Char, String) -> Bool
validPostition (low, high, c, s) =
  high <= length s
    && if s !! (low - 1) == c
      then s !! (high - 1) /= c
      else s !! (high - 1) == c

newtype Parser a = Parser {runParser :: String -> (a, String)}

parseInputLow :: String -> (String, String)
parseInputLow ('-' : xs) = ([], xs)
parseInputLow (x : xs) = let (c, s) = parseInputLow xs in (x : c, s)

parserInputLow :: Parser Int
parserInputLow = Parser $ \s -> let (c, s') = parseInputLow s in (read c, s')

parseInputHigh :: String -> (String, String)
parseInputHigh (' ' : xs) = ([], xs)
parseInputHigh (x : xs) = let (c, s) = parseInputHigh xs in (x : c, s)

parseInputChar :: String -> (Char, String)
parseInputChar s = (head s, drop 2 s)

parseInputString :: String -> (String, String)
parseInputString [] = ([], [])
parseInputString (' ' : xs) = parseInputString xs
parseInputString (x : xs) = let (c, s) = parseInputString xs in (x : c, s)

parseInput :: String -> (Int, Int, Char, String)
parseInput s =
  let (low, s') = parseInputLow s
      (high, s'') = parseInputHigh s'
      (char, s''') = parseInputChar s''
      (str, _) = parseInputString s'''
   in (read low, read high, char, str)

part1 :: [String] -> Int
part1 = length . filter (valid . parseInput)

part2 :: [String] -> Int
part2 = length . filter (validPostition . parseInput)

solve :: IO ()
solve = do
  file <- readInput 2
  let input = T.unpack <$> T.splitOn "\n" file
  print $ part1 input
  print $ part2 input

sampleInput = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
