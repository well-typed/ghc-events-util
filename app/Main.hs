module Main where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Word
import GHC.RTS.Events
import System.Exit
import System.IO
import Text.Printf

import Cmdline

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdCommand cmdline of
      ShowDelta padding -> showDelta cmdline padding

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

showDelta :: Cmdline -> Padding -> IO ()
showDelta cmdline padding = do
    EventLog{header, dat} <- readEventLogOrExit (cmdInput cmdline)
    go (buildEventTypeMap $ eventTypes header) (sortEvents $ events dat)
  where
    Padding{padDelta, padTime, padCap} = padding

    go :: IntMap EventType -> [Event] -> IO ()
    go imap = loop 0
      where
        loop :: Timestamp -> [Event] -> IO ()
        loop _    []     = return ()
        loop prev (e:es) = do
            let diffNs :: Word64
                diffNs = evTime e - prev

                diffMs :: Double
                diffMs = fromIntegral diffNs / 1_000_000
            putStrLn $ mconcat [
                padTo padDelta $
                  printf "%7.2fms" diffMs
              , padTo padTime $
                  show (evTime e)
              , padTo padCap $
                   maybe "" (\c -> "cap " ++ show c) (evCap e)
              , autoIndent (totalPadding padding) $
                  showEventInfoWith imap (evSpec e)
              ]
            loop (evTime e) es

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

