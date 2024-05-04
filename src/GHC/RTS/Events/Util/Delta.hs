{-# LANGUAGE OverloadedStrings #-}

-- | Time difference (delta) between events
--
-- Intended for unqualified import.
module GHC.RTS.Events.Util.Delta (
    Delta(..)
  , computeDelta
  ) where

import Data.Proxy
import Data.Tree
import Data.Word
import GHC.RTS.Events

import GHC.RTS.Events.Util.Decorated

{-------------------------------------------------------------------------------
  Main definition
-------------------------------------------------------------------------------}

-- | Time interval (in ns) between an event and the /next/
data Delta =
    Delta Word64
  | NoDelta -- ^ Used only for the final event (per capability)
  deriving (Show)

-- | Compute the Delta for each event
--
-- The final event will have a delta of 0.
computeDelta :: forall s ds.
     Proxy s
  -> [Decorated ds Event]
  -> [Decorated ('(s, Delta):ds) Event]
computeDelta _ =
    fst . go
  where
    go ::
         [Decorated ds Event]
      -> ( [Decorated ('(s, Delta) : ds) Event]
         , FirstTimestamps
         )
    go []     = ([], firstEmpty)
    go (d:ds) = (
          addDecoration' delta d : ds'
        , firstUpdate e first
        )
      where
        e :: Event
        e = stripDecoration d

        ds'   :: [Decorated ('(s, Delta) : ds) Event]
        first :: FirstTimestamps
        ~(ds', first) = go ds

        delta :: Delta
        delta = case firstLookup e first of
                  Nothing -> NoDelta
                  Just t  -> Delta (t - evTime e)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Per capability, the timestamp of the /first/ event
--
-- We do not record timestamps for events that are not associated with a
-- capability. The reason is that the eventlog looks something like this:
--
-- > .. a few setup events (not associated with capabilities)
-- > .. many actual events (associated with capabilities)
-- > .. a few teardown events (not associated with capabilities)
--
-- If we were to try to compute deltas for events not associated with
-- capabilities, we would have to look very far ahead to compute the delta
-- between the final setup event and the first teardown event, thereby
-- destroying incrementality.
type FirstTimestamps = LazyIntMap Timestamp

firstEmpty :: FirstTimestamps
firstEmpty = lazyEmpty

firstLookup :: Event -> FirstTimestamps -> Maybe Timestamp
firstLookup e first = flip lazyLookup first =<< capability e

firstUpdate :: Event -> FirstTimestamps -> FirstTimestamps
firstUpdate e = maybe id (\cap -> lazyInsert cap (evTime e)) (capability e)

-- | Capability associated with an event
--
-- We associate 'CapDelete' events with the capability they are delete. This
-- provides a useful final event for computing deltas.
capability :: Event -> Maybe Int
capability e
  | CapDelete c <- evSpec e = Just c
  | otherwise               = evCap e

{-------------------------------------------------------------------------------
  Internal documentation

  This checks that we are sufficiently lazy.
-------------------------------------------------------------------------------}

_exampleEvents :: [Decorated '[] Event]
_exampleEvents = map undecorated $
    emptyEvent { evTime = 0  }
  : emptyEvent { evTime = 10 }
  : undefined

_computedEvents :: [Decorated '[ '("delta", Delta) ] Event]
_computedEvents = take 1 $ computeDelta (Proxy @"delta") _exampleEvents

emptyEvent :: Event
emptyEvent = Event {
      evTime = 0
    , evSpec = UserMessage ""
    , evCap  = Nothing
    }

{-------------------------------------------------------------------------------
  Internal: lazy map

  This is a rather specialized data structure. We want to be able to insert
  elements into the map /independent of the existing map structure/ (otherwise
  we do not get the laziness properties we need). This means that every key
  must be given a /specific/ position in the map; this will only work for maps
  over small domains. In our case, this is fine, we are using this to store
  timestamps per capability, which will be a small domain.

  The indices 0..14 (for a complete tree of depth 4) look like this
  (omitting all the Empty leaf nodes:

  > Just 0
  > |
  > +- Just 1
  > |  |
  > |  +- Just 3
  > |  |  |
  > |  |  +- Just 7
  > |  |  |
  > |  |  `- Just 11
  > |  |
  > |  `- Just 5
  > |     |
  > |     +- Just 9
  > |     |
  > |     `- Just 13
  > |
  > `- Just 2
  >    |
  >    +- Just 4
  >    |  |
  >    |  +- Just 8
  >    |  |
  >    |  `- Just 12
  >    |
  >    `- Just 6
  >       |
  >       +- Just 10
  >       |
  >       `- Just 14

  Inserting just _one_ of these indices into an empty map will construct the
  part of the map that is necessary. For example, inserting element 13:

  > Nothing
  > |
  > +- Nothing
  > |  |
  > |  +- Empty
  > |  |
  > |  `- Nothing
  > |     |
  > |     +- Empty
  > |     |
  > |     `- Just 13
  > |
  > `- Empty
-------------------------------------------------------------------------------}

-- | 'IntMap' variation that is spine lazy
--
-- >    lazyLookup 1 (lazyInsert 1 'a' undefined)
-- > == Just 'a'
data LazyIntMap a = Empty | Branch (Maybe a) (LazyIntMap a) (LazyIntMap a)

-- | 'Show' instance for debugging only (does not generate valid Haskell)
instance Show a => Show (LazyIntMap a) where
  show = drawTree . toTree
    where
      toTree :: LazyIntMap a -> Tree String
      toTree Empty           = Node "Empty" []
      toTree (Branch ma l r) = Node (show ma) $ map toTree [l, r]

expand :: LazyIntMap a -> (Maybe a, LazyIntMap a, LazyIntMap a)
expand Empty           = (Nothing, Empty, Empty)
expand (Branch ma l r) = (ma,      l,     r    )

pattern Expand :: Maybe a -> LazyIntMap a -> LazyIntMap a -> LazyIntMap a
pattern Expand ma l r <- (expand -> (ma, l, r))

{-# COMPLETE Expand #-}

lazyEmpty :: LazyIntMap a
lazyEmpty = Empty

lazyInsert :: Int -> a -> LazyIntMap a -> LazyIntMap a
lazyInsert 0 a ~(Expand _  l r) = Branch (Just a) l r
lazyInsert n a ~(Expand ma l r)
  | odd n     = let l' = lazyInsert (n `div` 2    ) a l in Branch ma l' r
  | otherwise = let r' = lazyInsert (n `div` 2 - 1) a r in Branch ma l  r'

lazyLookup :: Int -> LazyIntMap a -> Maybe a
lazyLookup 0 ~(Expand ma _ _) = ma
lazyLookup n ~(Expand _  l r)
  | odd n     = lazyLookup (n `div` 2    ) l
  | otherwise = lazyLookup (n `div` 2 - 1) r
