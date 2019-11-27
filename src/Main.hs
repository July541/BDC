module Main where
import Words

main :: IO ()
main = do
    words <- loadData "data/1.txt"
    startBDC words