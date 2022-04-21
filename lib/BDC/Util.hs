module BDC.Util where

import           Data.Time (UTCTime, getCurrentTimeZone, utcToLocalTime)

-- | Convert `UTCTime` to `LocalTime` String.
utcToLocalTimeString :: UTCTime -> IO String
utcToLocalTimeString time = do
    zone <- getCurrentTimeZone
    let local = utcToLocalTime zone time
    pure $ show local
