module Words where

import System.IO
import Data.Char(toLower)

type WordPair = (String, String)
type WordPairs = [WordPair]

ch :: WordPair -> String
ch = fst

en :: WordPair -> String
en = snd

checkSpell :: WordPair -> String -> Bool
checkSpell w s = (map toLower $ en w) == (map toLower s)

printBaseInfo :: Int -> Int -> String
printBaseInfo finishedCount totalCount = "[" ++ show finishedCount ++ "/" ++ show totalCount ++ "]"

printRightResult :: IO ()
printRightResult = do
    putStrLn "Correct!"

printWrongResult :: String -> IO ()
printWrongResult rightAnswer = do
    putStrLn $ "Uncorrect, the right answer is: " ++ rightAnswer

oneWord :: WordPair -> Int -> Int -> IO()
oneWord word finishedCount totalCount = do
    putStrLn $ printBaseInfo finishedCount totalCount
    putStrLn $ ch word
    answer <- getLine
    if checkSpell word answer
               then printRightResult
               else printWrongResult $ en word

startBDCImpl :: WordPairs -> Int -> Int -> IO()
startBDCImpl [] _ _ = return ()
startBDCImpl (x:xs) finishedCount totalCount = do
    oneWord x finishedCount totalCount
    startBDCImpl xs (finishedCount + 1) totalCount

startBDC :: WordPairs -> IO()
startBDC words = startBDCImpl words 1 $ length words

loadData :: FilePath -> IO WordPairs
loadData path = do
    contents <- readFile path
    return $ map (\x -> (mergeWords $ init x, last x)) $ map words $ lines contents

mergeWords :: [String] -> String
mergeWords [] = ""
mergeWords (x:[]) = x
mergeWords (x:xs) = x ++ " " ++ mergeWords xs