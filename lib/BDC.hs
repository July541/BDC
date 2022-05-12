{-# LANGUAGE RecordWildCards #-}

module BDC where

import BDC.DataLoader (loadDataFromTxt)
import BDC.Logic (checkAll)
import BDC.Types (Arguments (..), BDCRecord (..))
import BDC.Util (utcToLocalTimeString)
import Control.Monad (when)
import Data.Default (def)
import qualified Data.Text as T
import Data.Time
  ( diffUTCTime
  , getCurrentTime
  , nominalDiffTimeToSeconds
  )
import System.Random.Shuffle (shuffleM)

-- | Run BDC with arguments.
run :: Arguments -> IO ()
run Arguments {..} = do
  -- Display start time
  current <- getCurrentTime
  utcToLocalTimeString current >>= putStrLn

  -- Load data and show data log if required
  (words, log) <- loadDataFromTxt (T.unpack dataPath)
  when verbose $ print log

  -- Process words
  words' <- if random then shuffleM words else pure words
  let words'' = if reversed then reverse words' else words'
  let words''' = case limited of
        Nothing -> words''
        Just n -> take n words''

  -- Recite
  BDCRecord {..} <- checkAll words'''

  -- Show statistic
  let span = diffUTCTime endTime startTime
  putStrLn $ (show $ nominalDiffTimeToSeconds span) ++ "s"
  putStrLn $ show correctSize ++ "/" ++ show wordSize

-- Run BDC with default paraments.
runWithDefault :: IO ()
runWithDefault = run def
