module Main where

import Control.Monad
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word
import GHC.RTS.Events
import System.Exit
import System.IO
import Text.Printf

import GhcEventsUtil.Cmdline
import GhcEventsUtil.Regex qualified as Regex

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdCommand cmdline of
      ShowDelta padding filters -> showDelta cmdline padding filters

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- | We record the time of the previous event /per capability/
type PrevTimes = Map (Maybe Int) Timestamp

showDelta :: Cmdline -> Padding -> Filters -> IO ()
showDelta cmdline padding filters = do
    EventLog{header, dat} <- readEventLogOrExit (cmdInput cmdline)
    go (buildEventTypeMap $ eventTypes header) (sortEvents $ events dat)
  where
    go :: IntMap EventType -> [Event] -> IO ()
    go imap = loop Map.empty
      where
        loop :: PrevTimes -> [Event] -> IO ()
        loop _    []     = return ()
        loop prev (e:es) = do
            when shouldShow $
              putStrLn $ mconcat [
                  padTo (padDelta padding) $
                    printf "%7.2fms" diffMs
                , padTo (padTime padding) $
                    show (evTime e)
                , padTo (padCap padding) $
                     maybe "" (\c -> "cap " ++ show c) (evCap e)
                , autoIndent (totalPadding padding) $
                    eventInfo
                ]
            loop (Map.insert (evCap e) (evTime e) prev) es
          where
            diffNs :: Word64
            diffNs = evTime e - fromMaybe 0 (Map.lookup (evCap e) prev)

            diffMs :: Double
            diffMs = fromIntegral diffNs / 1_000_000

            eventInfo :: String
            eventInfo = showEventInfoWith imap (evSpec e)

            shouldShow :: Bool
            shouldShow = and [
                  maybe True (\t  -> evTime e >= t) $ filterShowFrom  filters
                , maybe True (\t  -> evTime e <= t) $ filterShowUntil filters
                , maybe True (\re -> Regex.matchTest re eventInfo) $ filterMatch filters
                ]

showEventInfoWith :: IntMap EventType -> EventInfo -> String
showEventInfoWith imap info =
    case info of
      UnknownEvent{ref} ->
        maybe "Unknown event" ppEventType $
          IntMap.lookup (fromIntegral ref) imap
      _otherwise ->
        showEventInfo info

{-------------------------------------------------------------------------------
  Internal auxiliary: eventlog
-------------------------------------------------------------------------------}

readEventLogOrExit :: FilePath -> IO EventLog
readEventLogOrExit fp = do
    mEventlog <- readEventLogFromFile fp
    case mEventlog of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right eventlog ->
        return eventlog

{-------------------------------------------------------------------------------
  Internal auxiliary: padding
-------------------------------------------------------------------------------}

padTo :: Maybe Int -> String -> String
padTo Nothing  _ = ""
padTo (Just p) s = s ++ replicate (p - length s) ' '

-- | Simulation of vim's \"autoindent\"
--
-- Each new line starts at the current column
autoIndent :: Int -> String -> String
autoIndent n = concatMap aux
  where
    aux :: Char -> [Char]
    aux '\n' = "\n" ++ replicate n ' '
    aux c    = [c]

totalPadding :: Padding -> Int
totalPadding p = sum [
      fromMaybe 0 $ padDelta p
    , fromMaybe 0 $ padTime  p
    , fromMaybe 0 $ padCap   p
    ]

