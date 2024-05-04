module GHC.RTS.Events.Util.EventInfo (
    addEventInfo
  , addEventInfo'
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import GHC.RTS.Events

import GHC.RTS.Events.Util.Decorated

{-------------------------------------------------------------------------------
  Main definitions
-------------------------------------------------------------------------------}

addEventInfo ::
     IntMap EventType -- ^ See 'buildEventTypeMap'
  -> Decorated ds Event
  -> Decorated ('("eventInfo", String) : ds) Event
addEventInfo = decorateWith' . showEventInfoWith

-- | Convenience wrapper around 'addEventInfo'
addEventInfo' ::
     Header
  -> [Event]
  -> [Decorated '[ '("eventInfo", String) ] Event]
addEventInfo' header =
    map (addEventInfo imap . undecorated)
  where
    imap :: IntMap EventType
    imap = buildEventTypeMap $ eventTypes header

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

showEventInfoWith :: IntMap EventType -> Event -> String
showEventInfoWith imap e =
    case evSpec e of
      UnknownEvent{ref} -> maybe "Unknown event" ppEventType $
                             IntMap.lookup (fromIntegral ref) imap
      info              -> showEventInfo info

