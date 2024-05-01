-- | Event filters
--
-- Intended for qualified import.
--
-- > import GhcEventsUtil.Filters (Filters(..))
-- > import GhcEventsUtil.Filters qualified as Filters
module GhcEventsUtil.Filters (
    Filters(..)
    -- * Main API
  , allDisabled
  , showEvent
  ) where

import Data.Maybe (isNothing)
import GHC.RTS.Events

import GhcEventsUtil.Regex (Regex)
import GhcEventsUtil.Regex qualified as Regex

{-------------------------------------------------------------------------------
  Definition

  NOTE: When we add another field, we should also update 'allDisabled'.
-------------------------------------------------------------------------------}

data Filters = Filters {
      filterShowFrom  :: Maybe Timestamp
    , filterShowUntil :: Maybe Timestamp
    , filterMatch     :: Maybe Regex
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

allDisabled :: Filters -> Bool
allDisabled filters = and [
      isNothing $ filterShowFrom  filters
    , isNothing $ filterShowUntil filters
    , isNothing $ filterMatch     filters
    ]

showEvent :: Filters -> Event -> String -> Bool
showEvent filters e eventInfo = and [
      maybe True (\t  -> evTime e >= t) $ filterShowFrom  filters
    , maybe True (\t  -> evTime e <= t) $ filterShowUntil filters
    , maybe True (\re -> Regex.matchTest re eventInfo) $ filterMatch filters
    ]

