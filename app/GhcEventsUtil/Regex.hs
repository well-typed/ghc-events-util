-- | Thin wrapper around regular expressions
--
-- Intended for qualified import.
--
-- > import GhcEvensRegex.Regex (Regex)
-- > import GhcEvensRegex.Regex qualified as Regex
module GhcEventsUtil.Regex (
    Regex -- opaque
  , compile
    -- * Main API
  , matchTest
  ) where

import GHC.Show (appPrec)
import Text.Regex.PCRE qualified as PCRE

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Regex = Regex {
      regexString   :: String
    , regexCompiled :: PCRE.Regex
    }

compile :: String -> Regex
compile regexString = Regex{
      regexString
    , regexCompiled = PCRE.makeRegex regexString
    }

instance Show Regex where
  showsPrec p Regex{regexString} = showParen (p > appPrec) $
      showString "compile "
    . shows regexString

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

matchTest :: Regex -> String -> Bool
matchTest Regex{regexCompiled} = PCRE.matchTest regexCompiled
