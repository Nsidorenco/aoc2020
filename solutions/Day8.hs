{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Foldable (find)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import FileHelper

sampleInput =
  "nop +0 \n acc +1 \n jmp +4 \n acc +3 \n jmp -3 \n acc -99 \n acc +1 \n jmp -4 \n acc + 6\n" ::
    Text

parseLine :: Parser (Text, Int)
parseLine = do
  many' space
  op <- T.pack <$> many' letter
  many' space
  sign <- anyChar
  rest <- T.unpack <$> takeTill isEndOfLine
  if sign == '+'
    then return (op, read rest)
    else return (op, ((-) 0 . read) rest)

parseInput :: Parser [(Text, Int)]
parseInput = newlineSep parseLine
  where
    newlineSep p = p `sepBy` char '\n'

runMachine :: Int -> Int -> Set Int -> [(Text, Int)] -> Int
runMachine acc i visited commands =
  if i `S.member` visited || length commands <= i then acc else recurse
  where
    recurse =
      case commands !! i of
        ("nop", _) -> runMachine acc (i + 1) (i `S.insert` visited) commands
        ("acc", n) -> runMachine (acc + n) (i + 1) (i `S.insert` visited) commands
        ("jmp", n) -> runMachine acc (i + n) (i `S.insert` visited) commands

runMachinePath :: Int -> Int -> Set Int -> [(Text, Int)] -> Maybe Int
runMachinePath acc i visited commands =
  if i `S.member` visited
    then Nothing
    else if length commands <= i then Just acc else recurse
  where
    recurse =
      case commands !! i of
        ("nop", _) -> runMachinePath acc (i + 1) (i `S.insert` visited) commands
        ("acc", n) -> runMachinePath (acc + n) (i + 1) (i `S.insert` visited) commands
        ("jmp", n) -> runMachinePath acc (i + n) (i `S.insert` visited) commands

replaceAtIndex :: [(Text, Int)] -> Int -> [(Text, Int)]
replaceAtIndex commands i =
  List.take i commands ++ [newElem] ++ drop (i + 1) commands
  where
    newElem = case commands !! i of
      ("jmp", n) -> ("nop", n)
      ("nop", n) -> ("jmp", n)
      n -> n

part1 :: [(Text, Int)] -> Int
part1 = runMachine 0 0 S.empty

part2 :: [(Text, Int)] -> Int
part2 commands =
  head $ mapMaybe (runMachinePath 0 0 S.empty) commands'
  where
    commands' =
      [replaceAtIndex commands i | i <- [0 .. length commands], fst (commands !! i) /= "acc"]

solve :: IO ()
solve = do
  file <- readInput 8
  let input = parseOnly parseInput file
  case input of
    (Right x) -> do
      print $ part1 x
      print $ part2 x
    _ -> print input
