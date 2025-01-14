module Command.ShowDelta (showDelta) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.RTS.Events.Util

import Cmdline

{-------------------------------------------------------------------------------
  Command: 'ShowDelta'
-------------------------------------------------------------------------------}

showDelta :: Cmdline -> IO ()
showDelta cmdline = do
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

    showFiltered cmdline withDelta
  where
    Cmdline{cmdInput, cmdMaxLookahead, cmdSort} = cmdline

showFiltered :: forall ds.
     ( ApplyPadding (Decorated ds Event)
     , HasDecoration "eventInfo" ds Text
     )
  => Cmdline -> [Decorated ds Event] -> IO ()
showFiltered cmdline events =
    if allFiltersDisabled cmdFilters then
      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents cmdPadding events
    else do
      -- Apply user-specified cmdFilters, but leave the CapDelete events, so that
      -- we have a final event to compute deltas against.
      let filtered :: [Decorated ds Event]
          filtered = filter (filterEvent cmdFilters) events

      -- Compute new deltas, now between the /shown/ events
      let withShownDelta ::
            [ Decorated ('("shownDelta", Delta) ': ds)  Event ]
          withShownDelta = computeDelta (Proxy @"shownDelta") filtered

      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents cmdPadding withShownDelta
  where
    Cmdline{cmdFilters, cmdPadding} = cmdline



