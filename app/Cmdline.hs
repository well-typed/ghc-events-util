module Cmdline (
    Cmdline(..)
  , Command(..)
  , Padding(..)
  , getCmdline
  ) where

import Options.Applicative

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdCommand :: Command
    , cmdInput   :: FilePath
    }
  deriving (Show)

data Command =
    ShowDelta Padding
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
      <$> parseCommand
      <*> argument str (metavar "FILE")

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      cmd "show-delta" parseShowDelta $
        "Pretty print an event log, showing time intervals between events"
    ]
  where
    cmd :: String -> Parser Command -> String -> Mod CommandFields Command
    cmd l p d = command l $ info (p <**> helper) (progDesc d)

parseShowDelta :: Parser Command
parseShowDelta = ShowDelta <$> parsePadding

parsePadding :: Parser Padding
parsePadding =
    Padding
      <$> parsePaddingFor "delta" 10
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



