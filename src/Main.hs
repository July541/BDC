module Main where

import BDC (run)
import Options.Applicative
import Parser (argumentsParser)

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (argumentsParser <**> helper)
        ( fullDesc
            <> progDesc "Bei dan ci"
        )
