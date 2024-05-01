module GhcEventsUtil.Cmdline (
    Cmdline(..)
  , Command(..)
  , Padding(..)
  , Filters(..)
  , getCmdline
  ) where

import GHC.RTS.Events (Timestamp)
import Options.Applicative

import GhcEventsUtil.Regex (Regex)
import GhcEventsUtil.Regex qualified as Regex

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdInput   :: FilePath
    , cmdCommand :: Command
    }
  deriving (Show)

data Command =
    ShowDelta Padding Filters
  deriving (Show)

-- | Padding (in characters) for the fields
--
-- 'Nothing' indicates a field should be be skipped.
data Padding = Padding {
      padDelta :: Maybe Int
    , padTime  :: Maybe Int
    , padCap   :: Maybe Int
    }
  deriving (Show)

data Filters = Filters {
      filterShowFrom  :: Maybe Timestamp
    , filterShowUntil :: Maybe Timestamp
    , filterMatch     :: Maybe Regex
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = execParser opts
  where
    opts = info (parseCmdline <**> helper) $ mconcat [
          fullDesc
        , progDesc "Utilities for working with the GHC eventlog"
        ]

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    Cmdline
      <$> argument str (metavar "FILE")
      <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      cmd "show-delta" parseShowDelta $
        "Pretty print an event log, showing time intervals between events"
    ]
  where
    cmd :: String -> Parser Command -> String -> Mod CommandFields Command
    cmd l p d = command l $ info (p <**> helper) (progDesc d)

parseShowDelta :: Parser Command
parseShowDelta = ShowDelta <$> parsePadding <*> parseFilters

parsePadding :: Parser Padding
parsePadding =
    Padding
      <$> parsePaddingFor "delta" 11
      <*> parsePaddingFor "time"  13
      <*> parsePaddingFor "cap"   7

parsePaddingFor :: String -> Int -> Parser (Maybe Int)
parsePaddingFor field def = asum [
      flag' Nothing $ mconcat [
          long $ "skip-" ++ field
        , help $ "Omit field " ++ show field
        ]
    , fmap Just $ option auto $ mconcat [
          long $ "pad-" ++ field
        , help $ " Set padding for field " ++ show field
        , showDefault
        , value def
        ]
    ]

parseFilters :: Parser Filters
parseFilters =
    Filters
      <$> (optional $ option auto $ mconcat [
               long "show-from"
             , help "Timestamp of the first event to show"
             ])
      <*> (optional $ option auto $ mconcat [
               long "show-until"
             , help "Timestamp of the last event to show"
             ])
      <*> (optional $ fmap Regex.compile $ strOption $ mconcat [
               long "match"
             , help "Only show events that match"
             , metavar "REGEX"
             ])




