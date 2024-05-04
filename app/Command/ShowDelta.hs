module Command.ShowDelta (showDelta) where

import Data.Text.IO qualified as Text
import GHC.RTS.Events.Util

import Cmdline
import Data.Text (Text)

{-------------------------------------------------------------------------------
  Command: 'ShowDelta'
-------------------------------------------------------------------------------}

showDelta :: Cmdline -> Padding -> Filters -> IO ()
showDelta cmdline padding filters = do
    (header, events) <- readEventLogIncremental (cmdInput cmdline)

    let withInfo :: [ Decorated '[ '("eventInfo", Text) ] Event ]
        withInfo = addEventInfo' header $
                     if cmdSort cmdline
                       then sortEvents events
                       else events

    -- We compute the real delta before doing any filtering
    let withDelta ::
          [ Decorated '[ '("realDelta", Delta)
                       , '("eventInfo", Text)
                       ] Event
          ]
        withDelta = computeDelta (Proxy @"realDelta") withInfo

    if allFiltersDisabled filters then
      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents padding withDelta
    else do
      -- Apply user-specified filters, but leave the CapDelete events, so that
      -- we have a final event to compute deltas against.
      let filtered ::
            [ Decorated '[ '("realDelta", Delta)
                         , '("eventInfo", Text)
                         ] Event
            ]
          filtered = filter (filterEvent filters) withDelta

      -- Compute new deltas, now between the /shown/ events
      let withShownDelta ::
            [ Decorated '[ '("shownDelta", Delta)
                         , '("realDelta", Delta)
                         , '("eventInfo", Text)
                         ] Event
            ]
          withShownDelta = computeDelta (Proxy @"shownDelta") filtered

      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents padding withShownDelta