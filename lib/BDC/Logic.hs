{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module BDC.Logic where

import BDC.Types
  ( BDCEnv
  , BDCRecord (..)
  , Recited (Recited)
  , Word (..)
  , initialBDCRecord
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Console.ANSI
  ( Color (Green, Red, Yellow)
  , ColorIntensity (Vivid)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor)
  , setSGR
  )
import Prelude hiding (Word)

checkAll :: [Word] -> IO BDCRecord
checkAll words = do
  t <- getCurrentTime
  snd <$> runStateT (mapM checkOneWord words) (initialBDCRecord t)

getUserInput :: IO String
getUserInput = getLine

printSource :: T.Text -> IO ()
printSource = putStrLn . T.unpack

printSuccess :: IO ()
printSuccess = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Correct!"
  setSGR [Reset]

printFail :: T.Text -> IO ()
printFail s = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "Oops, right answer is: "
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ T.unpack s
  setSGR [Reset]

checkOneWord :: MonadIO m => Word -> BDCEnv m
checkOneWord word = do
  liftIO $ printSource $ word.from
  input <- liftIO $ T.pack <$> getUserInput

  currentTime <- liftIO $ getCurrentTime

  if input == word.to
    then updateSuccess input currentTime >> liftIO printSuccess
    else updateFail input currentTime >> liftIO (printFail word.to)
  where
    update f g input currentTime = do
      BDCRecord {..} <- get
      let recited = Recited word input currentTime
      put
        ( BDCRecord
            { reciteRecord = recited : reciteRecord
            , correctSize = f correctSize
            , errorSize = g errorSize
            , wordSize = wordSize + 1
            , startTime = startTime
            , endTime = currentTime
            }
        )
    updateSuccess = update (+ 1) id
    updateFail = update id (+ 1)

checkOneWordWithRepeat :: MonadIO m => Word -> BDCEnv m
checkOneWordWithRepeat = undefined
