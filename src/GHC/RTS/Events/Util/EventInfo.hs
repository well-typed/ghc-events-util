{-# LANGUAGE OverloadedStrings #-}

module GHC.RTS.Events.Util.EventInfo (
    addEventInfo
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text.Lazy qualified as Text.Lazy
import GHC.RTS.Events

-- Match the imports in GHC.RTS.Events, so that we can easily copy/paste cases
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB

import GHC.RTS.Events.Util.Decorated

{-------------------------------------------------------------------------------
  Main definitions
-------------------------------------------------------------------------------}

addEventInfo ::
     Header
  -> Int    -- ^ Max look-ahead (looking for labels for new threads)
  -> [Event]
  -> [Decorated '[ '("eventInfo", Text) ] Event]
addEventInfo header maxLookahead = loop IntMap.empty
  where
    imap :: IntMap EventType
    imap = buildEventTypeMap $ eventTypes header

    loop :: ThreadLabels -> [Event] -> [Decorated '[ '("eventInfo", Text)] Event]
    loop _            []     = []
    loop threadLabels (e:es) =
          addDecoration' eventInfo (undecorated e)
        : loop (dropFinishedThreads e threadLabels') es
      where
        threadLabels' :: ThreadLabels
        threadLabels' = updateThreadLabels e (take maxLookahead es) threadLabels

        eventInfo :: Text
        eventInfo =
            Text.Lazy.toStrict . TB.toLazyText $
              buildEventInfoWith imap threadLabels' e

{-------------------------------------------------------------------------------
  Thread labels
-------------------------------------------------------------------------------}

type ThreadLabels = IntMap Text

updateThreadLabels ::
     Event    -- ^ Current event
  -> [Event]  -- ^ Available look-ahead (used when we see 'CreateThread')
  -> ThreadLabels -> ThreadLabels
updateThreadLabels = \e es ->
    case evSpec e of
      CreateThread thread ->
        lookAhead thread es
      ThreadLabel thread label ->
        IntMap.insert (fromIntegral thread) label
      _otherwise ->
        id
  where
    lookAhead :: ThreadId -> [Event] -> ThreadLabels -> ThreadLabels
    lookAhead thread = go
      where
        go :: [Event] -> ThreadLabels -> ThreadLabels
        go []     = id
        go (e:es) =
            case evSpec e of
              ThreadLabel thread' label | thread == thread' ->
                IntMap.insert (fromIntegral thread) label
              _otherwise ->
                go es

dropFinishedThreads :: Event -> ThreadLabels -> ThreadLabels
dropFinishedThreads e =
    case evSpec e of
      StopThread thread ThreadFinished ->
        IntMap.delete (fromIntegral thread)
      _otherwise ->
        id

{-------------------------------------------------------------------------------
  Build event info
-------------------------------------------------------------------------------}

-- | Build event info
--
-- We override all cases that involve thread IDs, so that we can show the thread
-- label. For the remainder of the remainders we default to 'showEventInfo', so
-- that this should continue to work without changes even if new events are
-- added.
buildEventInfoWith :: IntMap EventType -> ThreadLabels -> Event -> TB.Builder
buildEventInfoWith imap threadLabels e =
    case evSpec e of
      -- user events
      UnknownEvent{ref} ->
        -- buildEventType is not exported from ghc-events
        maybe "Unknown event" (TB.fromString . ppEventType) $
          IntMap.lookup (fromIntegral ref) imap

      -- thread scheduling
      CreateThread thread ->
        "creating thread " <> buildThreadId thread
      RunThread thread ->
        "running thread " <> buildThreadId thread
      StopThread thread status ->
        "stopping thread " <> buildThreadId thread
        <> " (" <> TB.fromString (showThreadStopStatus status) <> ")"
      ThreadRunnable thread ->
        "thread " <> buildThreadId thread <> " is runnable"
      MigrateThread thread newCap  ->
        "migrating thread " <> buildThreadId thread
        <> " to cap " <> TB.decimal newCap
      WakeupThread thread otherCap ->
        "waking up thread " <> buildThreadId thread
        <> " on cap " <> TB.decimal otherCap
      ThreadLabel thread label ->
        "thread " <> TB.decimal thread -- not using buildThreadId here
        <> " has label \"" <> TB.fromText label <> "\""

      -- par sparks
      CreateSparkThread sparkThread ->
        "creating spark thread " <> TB.decimal sparkThread

      -- Haskell processes mgmt (thread groups that share heap and communicate)
      AssignThreadToProcess thread process ->
        "assigning thread " <> buildThreadId thread
        <> " to process " <> TB.decimal process

      -- communication between processes
      SendMessage mesTag senderProcess senderThread
        receiverMachine receiverProcess receiverInport ->
          "sending message with tag " <> TB.fromString (show mesTag)
          <> " from process " <> TB.decimal senderProcess
          <> ", thread " <> TB.decimal senderThread
          <> " to machine " <> TB.decimal receiverMachine
          <> ", process " <> TB.decimal receiverProcess
          <> " on inport " <> TB.decimal receiverInport
      ReceiveMessage mesTag receiverProcess receiverInport
        senderMachine senderProcess senderThread messageSize ->
          "receiving message with tag " <> TB.fromString (show mesTag)
          <> " at process " <> TB.decimal receiverProcess
          <> ", inport " <> TB.decimal receiverInport
          <> " from machine " <> TB.decimal senderMachine
          <> ", process " <> TB.decimal senderProcess
          <> ", thread " <> TB.decimal senderThread
          <> " with size " <> TB.decimal messageSize
      SendReceiveLocalMessage mesTag senderProcess senderThread
        receiverProcess receiverInport ->
          "sending/receiving message with tag " <> TB.fromString (show mesTag)
          <> " from process " <> TB.decimal senderProcess
          <> ", thread " <> TB.decimal senderThread
          <> " to process " <> TB.decimal receiverProcess
          <> " on inport " <> TB.decimal receiverInport

      -- default to 'buildEventInfo'
      info ->
        buildEventInfo info
  where
    buildThreadId :: ThreadId -> TB.Builder
    buildThreadId threadId =
        case IntMap.lookup (fromIntegral threadId) threadLabels of
          Nothing    -> TB.decimal threadId
          Just label -> mconcat [
              TB.decimal threadId
            , " \""
            , TB.fromText label
            , "\""
            ]