module Cmdline (
    Cmdline(..)
  , Command(..)
  , Padding(..)
  , Filters(..)
  , getCmdline
  , withCmdOutputHandle
  ) where

import GHC.RTS.Events.Util (Padding(..), Filters(..))
import Options.Applicative
import System.IO

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdInput   :: FilePath
    , cmdOutput  :: Maybe FilePath
    , cmdSort    :: Bool
    , cmdCommand :: Command
    }
  deriving (Show)

withCmdOutputHandle :: Cmdline -> (Handle -> IO a) -> IO a
withCmdOutputHandle Cmdline{cmdOutput} k =
    case cmdOutput of
      Nothing -> k stdout
      Just fp -> withFile fp WriteMode $ k

data Command =
    ShowDelta Padding Filters
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
      <*> (optional $ strOption $ mconcat [
               short 'o'
             , metavar "FILE"
             , help "Write output to a file"
             ])
      <*> (switch $ mconcat [
              long "sort-events"
            , help "Sort events before processing (this means processing is no longer incremental)"
            ])
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
      <*> parsePaddingFor "time"  14
      <*> parsePaddingFor "cap"   7

parsePaddingFor :: String -> Int -> Parser Int
parsePaddingFor field def = option auto $ mconcat [
      long $ "pad-" ++ field
    , help $ " Set padding for field " ++ show field
    , showDefault
    , value def
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
      <*> (optional $ strOption $ mconcat [
               long "match"
             , help "Only show events that match"
             , metavar "REGEX"
             ])




