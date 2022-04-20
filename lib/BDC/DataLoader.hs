{-# LANGUAGE RecordWildCards   #-}
module BDC.DataLoader where

import           BDC.Types                 (LoadLog (..), Word (Word), Words)
import           Control.Monad             (forM)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.State (StateT (runStateT), modify, evalStateT)
import qualified Data.Text                 as T
import           Data.Time                 (getCurrentTime)

loadDataFromTxt' :: FilePath -> StateT LoadLog IO Words
loadDataFromTxt' path = do
    contents <- liftIO $ readFile path
    concat <$> forM (zip (lines contents) [1..]) (\(c, l) -> do
        t <- liftIO $ getCurrentTime
        case buildOneItem c l t of
            Left e -> updateLog e id >> pure []
            Right r -> do
                updateLog T.empty (+1)
                pure [r]
        )
    where
        buildOneItem content line curTime = case words content of
            [x, y] -> Right $ Word (T.pack x) Nothing (T.pack y) curTime
            [x, y, z] -> Right $ Word (T.pack x) (Just $ T.pack y) (T.pack z) curTime
            _ -> Left $ T.pack $ "Unresolved line(#" ++ show line ++ "): " ++ content ++ "\n"

        updateLog e f = modify (\LoadLog{..} -> LoadLog {
                total = total + 1,
                succeed = f succeed,
                logs = logs <> e
            })

loadDataFromTxt :: FilePath -> IO (Words, LoadLog)
loadDataFromTxt path = runStateT (loadDataFromTxt' path) mempty
