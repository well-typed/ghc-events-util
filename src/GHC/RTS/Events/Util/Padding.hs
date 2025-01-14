{-# LANGUAGE OverloadedStrings #-}

-- | Output alignment
--
-- Intended for unqualified import.
module GHC.RTS.Events.Util.Padding (
    Padding(..)
  , ApplyPadding(..)
  , showDecoratedEvents
    -- * Auxiliary
  , padTo
  ) where

import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.RTS.Events
import Text.Printf

import GHC.RTS.Events.Util.Decorated
import GHC.RTS.Events.Util.Delta

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Padding (in characters) for the fields
--
-- 'Nothing' indicates the corresponding field should be skipped.
data Padding = Padding {
      padDelta        :: Maybe Int
    , padTimestamp    :: Maybe Int
    , padCap          :: Maybe Int
    , padRawEventType :: Maybe Int
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

showDecoratedEvents :: forall ds.
     ApplyPadding (Decorated ds Event)
  => Padding
  -> [Decorated ds Event]
  -> [Text]
showDecoratedEvents padding =
    map (applyPadding padding totalPadding)
  where
    totalPadding :: Int
    totalPadding = computeTotalPadding padding (Proxy @(Decorated ds Event))

{-------------------------------------------------------------------------------
  Applying padding
-------------------------------------------------------------------------------}

-- | Total padding (all padding /before/ the event info)
--
-- We can compute this statically (so that we need to do it only once).
type TotalPadding = Int

-- | Apply padding
class ApplyPadding a where
  computeTotalPadding :: Padding -> Proxy a -> TotalPadding
  applyPadding :: Padding -> TotalPadding -> a -> Text

instance ApplyPadding (Decorated '[ '("eventInfo", Text) ] Event) where
  computeTotalPadding padding _ = sum [
        fromMaybe 0 $ padTimestamp padding
      , fromMaybe 0 $ padCap padding
      ]
  applyPadding padding totalPadding (DecorateWith eventInfo (Undecorated e)) = mconcat $
        ( padTo (padTimestamp padding) $
            printf "%12d" (evTime e)
        )
      : ( padTo (padCap padding) $
            maybe "" (\c -> "cap " ++ show c) (evCap e)
        )
      : autoIndent totalPadding eventInfo

instance ApplyPadding (Decorated ds a)
      => ApplyPadding (Decorated ('(s, Delta) : ds) a) where
  computeTotalPadding padding _ = sum [
        fromMaybe 0 $ padDelta padding
      , computeTotalPadding padding (Proxy @(Decorated ds a))
      ]
  applyPadding padding totalPadding (DecorateWith d a) =
     -- We need to be careful here: rendering the delta might trigger a
     -- computation which, depending on the enabled filters, might have to look
     -- ahead quite far. If we do this /before/ rendering the event, we will
     -- hang on to all events in between this one and the next shown event,
     -- resulting in memory usage which is is linear in the maximum distance
     -- between any two shown events. We avoid this problem if we render the
     -- event /first/, and only /then/ render the delta.
     let !withoutDelta = applyPadding padding totalPadding a
     in mconcat [
        padTo (padDelta padding) $ aux d
      , withoutDelta
      ]
    where
      aux :: Delta -> String
      aux NoDelta    = "   ---"
      aux (Delta ns) = printf "%7.2fms" ms
        where
          ms :: Double
          ms = fromIntegral ns / 1_000_000

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

padTo :: Maybe Int -> String -> Text
padTo Nothing  _ = mempty
padTo (Just p) s = Text.pack $ s ++ replicate (p - length s) ' '

-- | Simulation of vim's \"autoindent\"
--
-- Each new line starts at the current column
autoIndent :: Int -> Text -> [Text]
autoIndent n = zipWith addPadding [0..] . Text.lines
  where
    addPadding :: Int -> Text -> Text
    addPadding 0 t = t  -- don't add padding to the first line
    addPadding _ t = padding <> t

    padding :: Text
    padding = Text.pack $ replicate n ' '
