{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day18 where

import Control.Applicative (optional)
import Data.Attoparsec.Text (Parser, char, choice, decimal, endOfLine, isEndOfLine, parseOnly, parseTest, sepBy, skipSpace, space, string, takeTill)
import Data.Char (digitToInt, intToDigit)
import Data.Either (lefts, rights)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import qualified Data.Text as T
import FileHelper

data OP = Plus | Mult deriving (Show, Eq)

data Exp
  = BinOp OP Exp Exp
  | Parens Exp
  | Literal Int
  deriving (Show, Eq)

data AST
  = BinNode OP AST AST
  | LiteralNode Int
  deriving (Show, Eq)

simpleInput = T.filter (/= ' ') "1 + 2 * 3 + 4 * 5 + 6" :: T.Text

input1 = T.filter (/= ' ') "1 + (2 * 3) + (4 * (5 + 6))" :: T.Text

input2 = "2 * 3 + (4 * 5)" :: T.Text

input3 = "5 + (8 * 3 + 9 + 3 * 4 * 3)" :: T.Text

input4 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" :: T.Text

input5 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" :: T.Text

ast :: String -> AST
ast txt = go [] txt 0
  where
    go pre (x : xs) 0 = case x of
      '+' -> BinNode Plus (ast pre) (ast xs)
      '*' -> BinNode Mult (ast pre) (ast xs)
      _ -> go (x : pre) xs 0
    go pre (x : xs) n = case x of
      '(' -> go (x : pre) xs (n + 1)
      ')' -> go (x : pre) xs (n - 1)
    go _ [] _ = LiteralNode $ read txt

prepareInput :: T.Text -> T.Text
prepareInput = T.filter (/= ' ')

parseLiteral :: Parser Exp
parseLiteral = do
  n <- decimal
  return (Literal n)

parseParens :: Parser Exp
parseParens = do
  string "("

  exp <- parseExp

  string ")"

  return (Parens exp)

parseOp :: Parser OP
parseOp = choice [parsePlus, parseMult]
  where
    parsePlus = do
      char '+'
      return Plus
    parseMult = do
      char '*'
      return Mult

parseFactor :: Parser Exp
parseFactor = choice [parseLiteral, parseParens]

parseBinOp :: Parser Exp
parseBinOp = do
  n <- parseFactor
  op <- parseOp
  m <- parseExp

  return (BinOp op n m)

parseExp :: Parser Exp
parseExp = choice [parseBinOp, parseParens, parseLiteral]

computeExp :: Exp -> Int
computeExp exp =
  case exp of
    Literal n -> n
    Parens exp -> computeExp exp
    BinOp op exp1 exp2 -> computeBinOp op exp1 exp2
  where
    computeBinOp Plus exp1 exp2 =
      case exp2 of
        Literal n -> computeExp exp1 + n
        Parens exp -> computeExp exp1 + computeExp exp
        BinOp op exp1' exp2' -> computeBinOp op (Literal (computeExp exp1 + computeExp exp1')) exp2'
    computeBinOp Mult exp1 exp2 =
      case exp2 of
        Literal n -> computeExp exp1 * n
        Parens exp -> computeExp exp1 * computeExp exp
        BinOp op exp1' exp2' -> computeBinOp op (Literal (computeExp exp1 * computeExp exp1')) exp2'

computeExp' :: Exp -> Int
computeExp' exp =
  case exp of
    Literal n -> n
    Parens exp -> computeExp' exp
    BinOp op exp1 exp2 -> computeBinOp op exp1 exp2
  where
    computeBinOp Plus exp1 exp2 =
      case exp2 of
        Literal n -> computeExp' exp1 + n
        Parens exp -> computeExp' exp1 + computeExp' exp
        BinOp op exp1' exp2' -> computeBinOp op (Literal (computeExp' exp1 + computeExp' exp1')) exp2'
    computeBinOp Mult exp1 exp2 =
      case exp2 of
        Literal n -> computeExp' exp1 * n
        Parens exp -> computeExp' exp1 * computeExp' exp
        BinOp Mult exp1' exp2' -> computeBinOp Mult (Literal (computeExp' exp1 * computeExp' exp1')) exp2'
        BinOp Plus exp1' exp2' -> computeExp' exp1 * computeBinOp Plus exp1' exp2'

evaluate :: T.Text -> Int
evaluate txt = either (\_ -> - 1) computeExp (parseOnly parseExp (prepareInput txt))

evaluate' :: T.Text -> Int
evaluate' txt = either (\_ -> - 1) computeExp' (parseOnly parseExp (prepareInput txt))

solve = do
  file <- T.splitOn "\n" <$> readInput 18
  -- let file = sampleInput2
  print $ sum $ computeExp <$> rights (parseOnly parseExp . prepareInput <$> file)
  print $ sum $ computeExp' <$> rights (parseOnly parseExp . prepareInput <$> file)
