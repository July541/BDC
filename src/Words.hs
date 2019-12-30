module Words where

import Data.Char(toLower)

import Message    

data WordItem = WordItem { ch :: String, en :: String } deriving Show

type WordDict = [WordItem]

checkSpell :: String -> WordItem -> Bool
checkSpell input target = (toLower <$> input) == (toLower <$> en target)
                          
checkOneWord :: WordItem -> IO()
checkOneWord word = do
  putStrLn $ ch word
  answer <- getLine
  putStrLn $ if checkSpell answer word
             then rightMessage
             else wrongMessage $ en word

processOneWord :: [(Int, WordItem)] -> Int -> IO ()
processOneWord [] _ = return ()
processOneWord ((number, word):xs) totalCount = do
  putStrLn $ baseMessage number totalCount
  checkOneWord word
  processOneWord xs totalCount

processBDC :: WordDict -> IO()
processBDC dict = processOneWord numberedWords totalCount
  where
    totalCount = length dict
    numberedWords = zip [1..] dict
