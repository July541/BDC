module Main where

import Words
import DataLoader

main :: IO ()
main = do
  words <- loadData "data/1.txt"
  processBDC words
