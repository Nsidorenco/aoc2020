module FileHelper where

import qualified Data.Text as T
import qualified Data.Text.IO as T

readInput :: Int -> IO T.Text
readInput day = do
  file <- T.readFile $ "inputs/day" ++ (show day) ++ ".txt"
  return $ T.strip file
