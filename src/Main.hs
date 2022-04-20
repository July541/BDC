module Main where

import           BDC.DataLoader     (loadDataFromTxt)
import           BDC.Logic          (checkAll)
import           BDC.Types          (BDCRecord (correctSize, wordSize))
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> exitFailure
    (x:_) -> do
      (a, r) <- loadDataFromTxt x
      stat <- checkAll $ reverse a
      let c = correctSize stat
          s = wordSize stat
      putStrLn $ show c ++ "/" ++ show s
