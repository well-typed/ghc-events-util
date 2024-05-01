module GhcEventsUtil.Command.ShowDelta (showDelta) where

import Control.Monad
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.RTS.Events
import System.Exit
import System.IO
import Text.Printf

import GhcEventsUtil.Cmdline
import GhcEventsUtil.Filters qualified as Filters

{-------------------------------------------------------------------------------
  Command: 'ShowDelta'
-------------------------------------------------------------------------------}

data PrevTimes = PrevTimes {
      -- | Per capability, the timestamp of the /actual/ previous event
      prevActual :: Map (Maybe Int) Timestamp

      -- | Per capability, the timestamp of the /shown/ previous event
    , prevShown :: Map (Maybe Int) Timestamp
    }

initPrevTimes :: PrevTimes
initPrevTimes = PrevTimes {
      prevActual = Map.empty
    , prevShown  = Map.empty
    }

-- | Time interval in msc between the previous event and the current
--
-- 'Nothing' if there is no previous event.
data Delta = Delta {
      deltaActual :: Maybe Double
    , deltaShown  :: Maybe Double
    }

deltaEvent :: PrevTimes -> Event -> Bool -> (PrevTimes, Delta)
deltaEvent PrevTimes{prevActual, prevShown} Event{evCap, evTime} isShown = (
      PrevTimes {
          prevActual = Map.insert evCap evTime prevActual
        , prevShown  = if isShown
                         then Map.insert evCap evTime prevShown
                         else                         prevShown
        }
    , Delta {
          deltaActual = toInterval <$> Map.lookup evCap prevActual
        , deltaShown  = toInterval <$> Map.lookup evCap prevShown
        }
    )
  where
    toInterval :: Timestamp -> Double
    toInterval prev = fromIntegral (evTime - prev) / 1_000_000

showDelta :: Cmdline -> Padding -> Filters -> IO ()
showDelta cmdline padding filters = do
    EventLog{header, dat} <- readEventLogOrExit (cmdInput cmdline)
    go (buildEventTypeMap $ eventTypes header) (sortEvents $ events dat)
  where
    go :: IntMap EventType -> [Event] -> IO ()
    go imap = loop initPrevTimes
      where
        loop :: PrevTimes -> [Event] -> IO ()
        loop _    []     = return ()
        loop prev (e:es) = do
            when isShown $
              putStrLn $ mconcat [
                  padTo (padDelta padding) $
                    maybe "   ---" (printf "%7.2fms") $ deltaShown delta
                , if Filters.allDisabled filters
                    then ""
                    else padTo (padDelta padding) $
                           maybe "---" (printf "%7.2fms") $ deltaActual delta
                , padTo (padTime padding) $
                    printf "%12d" (evTime e)
                , padTo (padCap padding) $
                     maybe "" (\c -> "cap " ++ show c) (evCap e)
                , autoIndent (totalPadding filters padding) $
                    eventInfo
                ]
            loop prev' es
          where
            eventInfo :: String
            eventInfo = showEventInfoWith imap (evSpec e)

            isShown :: Bool
            isShown = Filters.showEvent filters e eventInfo

            delta :: Delta
            prev' :: PrevTimes
            (prev', delta) = deltaEvent prev e isShown


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

totalPadding :: Filters -> Padding -> Int
totalPadding filters padding = sum [
      fromMaybe 0 $ padDelta padding
    , if Filters.allDisabled filters
        then 0
        else fromMaybe 0 $ padDelta padding
    , fromMaybe 0 $ padTime padding
    , fromMaybe 0 $ padCap padding
    ]

