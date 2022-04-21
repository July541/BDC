{-# LANGUAGE RecordWildCards #-}
module BDC.Types where

import           Control.Monad.Trans.State (StateT)
import           Data.Default              (Default (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import           Prelude                   hiding (Word)

type WordContent = Text

data Word = Word {
    from       :: WordContent,
    pos        :: Maybe WordContent,
    to         :: WordContent,
    createTime :: UTCTime
} deriving (Show)

type Words = [Word]

data Recited = Recited { -- ^ Recited word
    word      :: Word,
    userInput :: WordContent,
    inputTime :: UTCTime
}

data BDCRecord = BDCRecord {
    reciteRecord :: [Recited],
    correctSize  :: Int,
    errorSize    :: Int,
    wordSize     :: Int,
    startTime    :: UTCTime,
    endTime      :: UTCTime
}

-- data ReciteConfig = ReciteConfig {
--     from :: Language,
--     to :: Language,
--     wordPath :: FilePath
-- }

type BDCEnv m = StateT BDCRecord m ()

initialBDCRecord :: UTCTime -> BDCRecord
initialBDCRecord startTime =
    let endTime = startTime
        reciteRecord = []
        correctSize = 0
        errorSize = 0
        wordSize = 0
    in  BDCRecord{..}

data LoadLog = LoadLog {
    total   :: Int,
    succeed :: Int,
    logs    :: Text
} deriving (Show)

instance Semigroup LoadLog where
    ls <> rs = LoadLog {
        total = total ls + total rs,
        succeed = succeed ls + succeed rs,
        logs = logs ls <> logs rs
    }

instance Monoid LoadLog where
    mempty = LoadLog 0 0 T.empty

data Arguments = Arguments {
    dataPath           :: Text,
    reversed           :: Bool,
    random             :: Bool,
    repeatUntilCorrect :: Bool,
    verbose            :: Bool,
    limited            :: Maybe Int
    -- ^ Limited word count
} deriving (Show)

instance Default Arguments where
    def = Arguments {
        dataPath = T.pack "data/l1",
        reversed = False,
        random = True,
        repeatUntilCorrect = False,
        verbose = True,
        limited = Nothing
    }
