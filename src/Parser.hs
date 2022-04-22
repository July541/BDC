module Parser where
import Options.Applicative
import BDC.Types (Arguments (..))

argumentsParser :: Parser Arguments
argumentsParser = Arguments
    <$> strOption
        ( long "path"
        <> short 'p'
        <> metavar "TARGET"
        <> help "The path to load word list"
        )
    <*> switch
        ( long "random"
        <> help "Show words randomly"
        )
    <*> switch
        ( long "repeat"
        <> help "Repeat one word until correct"
        )
    <*> switch
        ( long "reverse"
        <> help "Reverse the word list"
        )
    <*> option (maybeReader (\x -> (pure . pure) (read x)))
        ( long "limit"
        <> showDefaultWith (const "Unlimited")
        <> value Nothing
        <> metavar "INT"
        <> help "Suppress maximum word count"
        )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "Verbose"
        )