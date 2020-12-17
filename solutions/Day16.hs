{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import Data.Attoparsec.Text as P
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, sortOn, transpose)
import qualified Data.Text as T
import FileHelper

sampleInput = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n \nyour ticket:\n7,1,14\n \nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12" :: T.Text

sampleInput2 = "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n \nyour ticket:\n11,12,13\n \nnearby tickets:\n3,9,18\n15,1,5\n5,14,9" :: T.Text

type Range = (Int, Int)

type Rule = (String, Range, Range)

type Ticket = [Int]

newLineSep p = p `sepBy` char '\n'

parseRules :: P.Parser [Rule]
parseRules = newLineSep parseRule
  where
    parseRule :: P.Parser (String, Range, Range)
    parseRule = do
      field <- T.unpack <$> P.takeWhile (/= ':')
      char ':'
      P.skipSpace
      l1 <- read <$> P.many' P.digit
      char '-'
      h1 <- read <$> many' P.digit
      P.skipSpace
      string "or"
      P.skipSpace
      l2 <- read <$> P.many' P.digit
      char '-'
      h2 <- read <$> P.many' P.digit

      return (field, (l1, h1), (l2, h2))

parseTicket :: P.Parser Ticket
parseTicket = commaSep P.decimal
  where
    commaSep p = p `P.sepBy` char ','

parseInput :: P.Parser ([Rule], Ticket, [Ticket])
parseInput = do
  rules <- parseRules
  P.many' endOfLine
  P.skipSpace
  P.many' endOfLine
  P.skipSpace
  P.many' endOfLine
  string "your ticket:\n"
  ticket <- parseTicket

  P.many' endOfLine
  P.skipSpace
  P.many' endOfLine
  P.skipSpace
  P.many' endOfLine
  string "nearby tickets:\n"
  tickets <- newLineSep parseTicket

  return (rules, ticket, tickets)

checkField :: [Rule] -> Int -> Bool
checkField rules val = any (\(_, (l1, h1), (l2, h2)) -> (l1 <= val && val <= h1) || (l2 <= val && val <= h2)) rules

checkTicket :: [Rule] -> Ticket -> [Int]
checkTicket rules = filter (not . checkField rules)

part1 :: ([Rule], Ticket, [Ticket]) -> Int
part1 (rules, _, tickets) =
  sum $ concatMap (checkTicket rules) tickets

checkRule :: [Int] -> Rule -> Bool
checkRule vals (_, (l1, h1), (l2, h2)) =
  all (\val -> (l1 <= val && val <= h1) || (l2 <= val && val <= h2)) vals

findField :: [Rule] -> [Int] -> ([String], Int)
findField rules vals =
  let possible = filter (checkRule vals) rules
   in (map (\(field, _, _) -> field) possible, head vals)

assignFields :: [([String], Int)] -> [(String, Int)]
assignFields fv =
  zip (foldl' (\acc field -> acc ++ [selectField acc field]) [] fields) vs
  where
    selectField :: [String] -> [String] -> String
    selectField taken possibilities =
      head $ filter (`notElem` taken) possibilities
    (fields, vs) = unzip $ sortOn (length . fst) fv

part2 :: ([Rule], Ticket, [Ticket]) -> Int
part2 (rules, ticket, tickets) =
  product $ map snd $ filter (\(field, _) -> "departure" `isPrefixOf` field) fields
  where
    fields = assignFields $ map (findField rules) (transpose valid)
    valid = filter (null . checkTicket rules) (ticket : tickets)

solve :: IO ()
solve = do
  file <- readInput 16
  let input = P.parseOnly parseInput file
  either print (print . part1) input
  either print (print . part2) input
