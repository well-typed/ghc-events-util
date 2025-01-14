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
      cmdOutput       :: Maybe FilePath
    , cmdSort         :: Bool
    , cmdResIsCap     :: Bool
    , cmdMaxLookahead :: Int
    , cmdPadding      :: Padding
    , cmdFilters      :: Filters
    , cmdCommand      :: Command
    , cmdInput        :: FilePath
    }
  deriving (Show)

withCmdOutputHandle :: Cmdline -> (Handle -> IO a) -> IO a
withCmdOutputHandle Cmdline{cmdOutput} k =
    case cmdOutput of
      Nothing -> k stdout
      Just fp -> withFile fp WriteMode $ k

data Command =
    ShowDelta
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
      <$> (optional $ strOption $ mconcat [
               short 'o'
             , metavar "FILE"
             , help "Write output to a file"
             ])
      <*> (switch $ mconcat [
              long "sort-events"
            , help "Sort events before processing (this means processing is no longer incremental)"
            ])
      <*> (switch $ mconcat [
              long "res-is-cap"
            , help "Re-interpret the 'residency' field in heap samples as the capability instead (for use with trace-foreign-calls)"
            ])
      <*> (option auto $ mconcat [
              long "max-lookahead"
            , help "Maximum lookahead (used to look for thread labels for newly created thrads)"
            , showDefault
            , value 100
            ])
      <*> parsePadding
      <*> parseFilters
      <*> parseCommand
      <*> argument str (metavar "FILE")

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      cmd "show-delta" (pure ShowDelta) $
        "Pretty print an event log, showing time intervals between events"
    ]
  where
    cmd :: String -> Parser Command -> String -> Mod CommandFields Command
    cmd l p d = command l $ info (p <**> helper) (progDesc d)

parsePadding :: Parser Padding
parsePadding =
    Padding
      <$> parsePaddingFor "delta"     (Just 11)
      <*> parsePaddingFor "timestamp" (Just 14)
      <*> parsePaddingFor "cap"       (Just 7)
      <*> parsePaddingFor "raw"       Nothing

parsePaddingFor :: String -> Maybe Int -> Parser (Maybe Int)
parsePaddingFor field (Just def) = asum [
      flag' Nothing $ mconcat [
          long $ "hide-" ++ field
        , help $ "Hide field " ++ show field
        ]
    , fmap Just $ option auto $ mconcat [
          long $ "pad-" ++ field
        , help $ "Set padding for field " ++ show field
        , showDefault
        , value def
        , metavar "PADDING"
        ]
    ]
parsePaddingFor field Nothing =
    optional $ option auto $ mconcat [
        long $ "show-" ++ field
      , help $ "Show field " ++ show field
      , metavar "PADDING"
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




