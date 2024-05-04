module Command.ShowDelta (showDelta) where

import Data.Text.IO qualified as Text
import GHC.RTS.Events.Util

import Cmdline
import Data.Text (Text)

{-------------------------------------------------------------------------------
  Command: 'ShowDelta'
-------------------------------------------------------------------------------}

showDelta :: Cmdline -> IO ()
showDelta cmdline@Cmdline{
              cmdInput
            , cmdMaxLookahead
            , cmdSort
            , cmdFilters
            , cmdPadding
            } = do
    (header, events) <- readEventLogIncremental cmdInput

    let withInfo :: [ Decorated '[ '("eventInfo", Text) ] Event ]
        withInfo = addEventInfo header cmdMaxLookahead $
                     if cmdSort
                       then sortEvents events
                       else events

    -- We compute the real delta before doing any filtering
    let withDelta ::
          [ Decorated '[ '("realDelta", Delta)
                       , '("eventInfo", Text)
                       ] Event
          ]
        withDelta = computeDelta (Proxy @"realDelta") withInfo

    if allFiltersDisabled cmdFilters then
      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents cmdPadding withDelta
    else do
      -- Apply user-specified cmdFilters, but leave the CapDelete events, so that
      -- we have a final event to compute deltas against.
      let filtered ::
            [ Decorated '[ '("realDelta", Delta)
                         , '("eventInfo", Text)
                         ] Event
            ]
          filtered = filter (filterEvent cmdFilters) withDelta

      -- Compute new deltas, now between the /shown/ events
      let withShownDelta ::
            [ Decorated '[ '("shownDelta", Delta)
                         , '("realDelta", Delta)
                         , '("eventInfo", Text)
                         ] Event
            ]
          withShownDelta = computeDelta (Proxy @"shownDelta") filtered

      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents cmdPadding withShownDelta