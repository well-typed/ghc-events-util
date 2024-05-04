-- | Event filters
--
-- Intended for unqualified import.
module GHC.RTS.Events.Util.Filter (
    Filters(..)
  , Regex -- opaque
    -- * Main API
  , allFiltersDisabled
  , filterEvent
  ) where

import Data.Maybe (isNothing)
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.RTS.Events
import GHC.Show (appPrec)
import Text.Regex.PCRE qualified as PCRE

import GHC.RTS.Events.Util.Decorated

{-------------------------------------------------------------------------------
  Definition

  NOTE: When we add another field, we should also update 'allFiltersDisabled'.
-------------------------------------------------------------------------------}

data Filters = Filters {
      filterShowFrom  :: Maybe Timestamp
    , filterShowUntil :: Maybe Timestamp
    , filterMatch     :: Maybe Regex
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Regex
-------------------------------------------------------------------------------}

-- | Simple wrapper around a 'PCRE.Regex' with support for a 'Show' instance
data Regex = Regex {
      regexString   :: String
    , regexCompiled :: PCRE.Regex
    }

instance Show Regex where
  showsPrec p Regex{regexString} = showParen (p > appPrec) $
      showString "compile "
    . shows regexString

instance IsString Regex where
  fromString regexString = Regex{
        regexString
      , regexCompiled = PCRE.makeRegex regexString
      }

matchTest :: Regex -> Text -> Bool
matchTest Regex{regexCompiled} = PCRE.matchTest regexCompiled . Text.unpack

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

allFiltersDisabled :: Filters -> Bool
allFiltersDisabled filters = and [
      isNothing $ filterShowFrom  filters
    , isNothing $ filterShowUntil filters
    , isNothing $ filterMatch     filters
    ]

-- | Apply the predicates
--
-- For 'CapDelete' events we always return 'True' so that we leave those events
-- to compute the final delta. See 'filterCapDelete'.
filterEvent ::
     HasDecoration "eventInfo" ds Text
  => Filters -> Decorated ds Event -> Bool
filterEvent filters d = or [
      case evSpec e of
        CapDelete _ -> True
        _otherwise  -> False
    , and [
          maybe True (\t  -> evTime e >= t) $ filterShowFrom  filters
        , maybe True (\t  -> evTime e <= t) $ filterShowUntil filters
        , maybe True (\re -> matchTest re eventInfo) $ filterMatch filters
        ]
    ]
  where
    e :: Event
    e = stripDecoration d

    eventInfo :: Text
    eventInfo = getDecoration (Proxy @"eventInfo") d
