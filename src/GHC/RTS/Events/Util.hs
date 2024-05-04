module GHC.RTS.Events.Util (
    -- * Read the eventlog
    readEventLogIncremental
    -- * Decorating events
  , Decorated(..)
    -- ** with event info
  , addEventInfo
  , addEventInfo'
    -- ** with Deltas (timestamp differences)
  , Delta(..)
  , computeDelta
    -- * Filtering events
  , Filters(..)
  , filterEvent
  , allFiltersDisabled
    -- * Generating output
  , Padding(..)
  , showDecoratedEvents
    -- * Convenience re-exports
  , module GHC.RTS.Events
  , module Data.Proxy
  ) where

import Data.Proxy

import GHC.RTS.Events
import GHC.RTS.Events.Util.Decorated
import GHC.RTS.Events.Util.Delta
import GHC.RTS.Events.Util.EventInfo
import GHC.RTS.Events.Util.Filter
import GHC.RTS.Events.Util.Incremental
import GHC.RTS.Events.Util.Padding
