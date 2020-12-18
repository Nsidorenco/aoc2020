{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

import Control.Applicative (optional)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, isEndOfLine, parseOnly, sepBy, space, string, takeTill)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import qualified Data.Text as T
import FileHelper
import Numeric (showIntAtBase)

sampleInput = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0" :: T.Text

sampleInput2 = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1" :: T.Text

mem [26] = 1

intToBin :: Int -> String
intToBin n = reverse $ take 36 $ reverse (showIntAtBase 2 intToDigit n "") ++ repeat '0'

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseAssignment :: Parser (Int, String)
parseAssignment = do
  optional space
  optional endOfLine
  string "mem["
  idx <- decimal
  string "] = "
  val <- decimal

  return (idx, intToBin val)

parseMask :: Parser (String, [(Int, String)])
parseMask = do
  string "mask = "
  mask <- takeTill isEndOfLine
  assignments <- newlineSep parseAssignment

  return (T.unpack mask, assignments)
  where
    newlineSep p = p `sepBy` char '\n'

parseInput :: Parser [(String, [(Int, String)])]
parseInput = newlineSep parseMask
  where
    newlineSep p = p `sepBy` char '\n'

applyMask :: String -> String -> String
applyMask = zipWith (\m x -> if m == 'X' then x else m)

updateMem :: Int -> String -> M.Map Int String -> M.Map Int String
updateMem = M.insert

runMask :: (String, [(Int, String)]) -> M.Map Int String -> M.Map Int String
runMask (mask, vals) map = foldl' (flip (uncurry updateMem)) map masked
  where
    masked = fmap (applyMask mask) <$> vals

runInstructions :: [(String, [(Int, String)])] -> M.Map Int String
runInstructions = foldl' (flip runMask) M.empty

part1 :: [(String, [(Int, String)])] -> Int
part1 = foldr ((+) . binToInt) 0 . runInstructions

permuteIndex :: String -> Int -> [Int]
permuteIndex mask idx = binToInt <$> applyFilter
  where
    applyFilter =
      ( mapM
          ( \(m, x) -> case m of
              'X' -> ['0', '1']
              '1' -> ['1']
              '0' -> [x]
          )
          (zip mask (intToBin idx))
      )

updateInstruction :: (String, [(Int, String)]) -> (String, [(Int, String)])
updateInstruction (mask, ins) = (mask, val)
  where
    val = ins >>= \(idx, x) -> (,x) <$> permuteIndex mask idx -- zip all new indexes with the store value

updateInstructions :: [(String, [(Int, String)])] -> [(String, [(Int, String)])]
updateInstructions = map updateInstruction

runMask2 :: (String, [(Int, String)]) -> M.Map Int String -> M.Map Int String
runMask2 (_, vals) map =
  foldl' (flip (uncurry updateMem)) map vals

runInstructions2 :: [(String, [(Int, String)])] -> M.Map Int String
runInstructions2 = foldl' (flip runMask2) M.empty

part2 :: [(String, [(Int, String)])] -> Int
part2 ins = foldr ((+) . binToInt) 0 (runInstructions2 (updateInstructions ins))

solve :: IO ()
solve = do
  file <- readInput 14
  -- let file = sampleInput2
  let input = parseOnly parseInput file
  either print (print . part1) input
  case input of
    Right x -> print $ part2 x
    _ -> error ""
