{-# LANGUAGE OverloadedStrings #-}

module GHC.RTS.Events.Util.EventInfo (
    addEventInfo
  , addEventInfo'
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as TB -- match import in GHC.RTS.Events
import GHC.RTS.Events

import GHC.RTS.Events.Util.Decorated

{-------------------------------------------------------------------------------
  Main definitions
-------------------------------------------------------------------------------}

addEventInfo ::
     IntMap EventType -- ^ See 'buildEventTypeMap'
  -> Decorated ds Event
  -> Decorated ('("eventInfo", Text) : ds) Event
addEventInfo = decorateWith' . showEventInfoWith

-- | Convenience wrapper around 'addEventInfo'
addEventInfo' ::
     Header
  -> [Event]
  -> [Decorated '[ '("eventInfo", Text) ] Event]
addEventInfo' header =
    map (addEventInfo imap . undecorated)
  where
    imap :: IntMap EventType
    imap = buildEventTypeMap $ eventTypes header

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

showEventInfoWith :: IntMap EventType -> Event -> Text
showEventInfoWith imap =
      Text.Lazy.toStrict
    . TB.toLazyText
    . buildEventInfoWith imap

buildEventInfoWith :: IntMap EventType -> Event -> TB.Builder
buildEventInfoWith imap e =
    case evSpec e of
      UnknownEvent{ref} ->
        -- buildEventType is not exported from ghc-events
        maybe "Unknown event" (TB.fromString . ppEventType) $
          IntMap.lookup (fromIntegral ref) imap
      info ->
        buildEventInfo info
