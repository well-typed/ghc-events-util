module Command.ShowDelta (showDelta) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.RTS.Events.Util

import Cmdline
import Data.Maybe (fromMaybe)

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
    let withDelta :: DecoratedEvents
        withDelta = DecoratedEvents $ computeDelta (Proxy @"realDelta") withInfo

    -- Add additional fields
    let withRawEventType :: DecoratedEvents
        withRawEventType = addRawEventType withDelta

    showFiltered cmdline withRawEventType
  where
    Cmdline{cmdInput, cmdMaxLookahead, cmdSort} = cmdline

{-------------------------------------------------------------------------------
  Processing events
-------------------------------------------------------------------------------}

data DecoratedEvents where
  DecoratedEvents ::
       ( ApplyPadding (Decorated ds Event)
       , HasDecoration "eventInfo" ds Text
       )
    => [Decorated ds Event]
    -> DecoratedEvents

showFiltered :: Cmdline -> DecoratedEvents -> IO ()
showFiltered cmdline (DecoratedEvents events) =
    if allFiltersDisabled cmdFilters then
      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents cmdPadding events
    else do
      -- Apply user-specified cmdFilters, but leave the CapDelete events, so that
      -- we have a final event to compute deltas against.
      let filtered = filter (filterEvent cmdFilters) events

      -- Compute new deltas, now between the /shown/ events
      let withShownDelta = computeDelta (Proxy @"shownDelta") filtered

      withCmdOutputHandle cmdline $ \h ->
        mapM_ (Text.hPutStrLn h) $ showDecoratedEvents cmdPadding withShownDelta
  where
    Cmdline{cmdFilters, cmdPadding} = cmdline

{-------------------------------------------------------------------------------
  Raw event types
-------------------------------------------------------------------------------}

newtype RawEventType = RawEventType {
      getRawEventType :: String
    }

addRawEventType :: DecoratedEvents -> DecoratedEvents
addRawEventType (DecoratedEvents events) = DecoratedEvents $
    map (decorateWith (Proxy @"rawType") (RawEventType . aux . evSpec)) events
  where
    aux :: EventInfo -> String
    aux AssignThreadToProcess{}     = "AssignThreadToProcess"
    aux BlocksSize{}                = "BlocksSize"
    aux CapCreate{}                 = "CapCreate"
    aux CapDelete{}                 = "CapDelete"
    aux CapDisable{}                = "CapDisable"
    aux CapEnable{}                 = "CapEnable"
    aux CapsetAssignCap{}           = "CapsetAssignCap"
    aux CapsetCreate{}              = "CapsetCreate"
    aux CapsetDelete{}              = "CapsetDelete"
    aux CapsetRemoveCap{}           = "CapsetRemoveCap"
    aux ConcMarkBegin{}             = "ConcMarkBegin"
    aux ConcMarkEnd{}               = "ConcMarkEnd"
    aux ConcSweepBegin{}            = "ConcSweepBegin"
    aux ConcSweepEnd{}              = "ConcSweepEnd"
    aux ConcSyncBegin{}             = "ConcSyncBegin"
    aux ConcSyncEnd{}               = "ConcSyncEnd"
    aux ConcUpdRemSetFlush{}        = "ConcUpdRemSetFlush"
    aux CreateMachine{}             = "CreateMachine"
    aux CreateProcess{}             = "CreateProcess"
    aux CreateSparkThread{}         = "CreateSparkThread"
    aux CreateThread{}              = "CreateThread"
    aux EdenEndReceive{}            = "EdenEndReceive"
    aux EdenStartReceive{}          = "EdenStartReceive"
    aux EndGC{}                     = "EndGC"
    aux EventBlock{}                = "EventBlock"
    aux GCDone{}                    = "GCDone"
    aux GCIdle{}                    = "GCIdle"
    aux GCStatsGHC{}                = "GCStatsGHC"
    aux GCWork{}                    = "GCWork"
    aux GlobalSyncGC{}              = "GlobalSyncGC"
    aux HeapAllocated{}             = "HeapAllocated"
    aux HeapBioProfSampleBegin{}    = "HeapBioProfSampleBegin"
    aux HeapInfoGHC{}               = "HeapInfoGHC"
    aux HeapLive{}                  = "HeapLive"
    aux HeapProfBegin{}             = "HeapProfBegin"
    aux HeapProfCostCentre{}        = "HeapProfCostCentre"
    aux HeapProfSampleBegin{}       = "HeapProfSampleBegin"
    aux HeapProfSampleCostCentre{}  = "HeapProfSampleCostCentre"
    aux HeapProfSampleEnd{}         = "HeapProfSampleEnd"
    aux HeapProfSampleString{}      = "HeapProfSampleString"
    aux HeapSize{}                  = "HeapSize"
    aux InfoTableProv{}             = "InfoTableProv"
    aux InternString{}              = "InternString"
    aux KillMachine{}               = "KillMachine"
    aux KillProcess{}               = "KillProcess"
    aux MemReturn{}                 = "MemReturn"
    aux MerCallingMain{}            = "MerCallingMain"
    aux MerCapSleeping{}            = "MerCapSleeping"
    aux MerCreateSpark{}            = "MerCreateSpark"
    aux MerEndParConjunct{}         = "MerEndParConjunct"
    aux MerEndParConjunction{}      = "MerEndParConjunction"
    aux MerFutureCreate{}           = "MerFutureCreate"
    aux MerFutureSignal{}           = "MerFutureSignal"
    aux MerFutureWaitNosuspend{}    = "MerFutureWaitNosuspend"
    aux MerFutureWaitSuspended{}    = "MerFutureWaitSuspended"
    aux MerLookingForGlobalThread{} = "MerLookingForGlobalThread"
    aux MerLookingForLocalSpark{}   = "MerLookingForLocalSpark"
    aux MerReleaseThread{}          = "MerReleaseThread"
    aux MerStartParConjunction{}    = "MerStartParConjunction"
    aux MerWorkStealing{}           = "MerWorkStealing"
    aux Message{}                   = "Message"
    aux MigrateThread{}             = "MigrateThread"
    aux NonmovingHeapCensus{}       = "NonmovingHeapCensus"
    aux NonmovingPrunedSegments{}   = "NonmovingPrunedSegments"
    aux OsProcessParentPid{}        = "OsProcessParentPid"
    aux OsProcessPid{}              = "OsProcessPid"
    aux PerfCounter{}               = "PerfCounter"
    aux PerfName{}                  = "PerfName"
    aux PerfTracepoint{}            = "PerfTracepoint"
    aux ProfBegin{}                 = "ProfBegin"
    aux ProfSampleCostCentre{}      = "ProfSampleCostCentre"
    aux ProgramArgs{}               = "ProgramArgs"
    aux ProgramEnv{}                = "ProgramEnv"
    aux ProgramInvocation{}         = "ProgramInvocation"
    aux ReceiveMessage{}            = "ReceiveMessage"
    aux RequestParGC{}              = "RequestParGC"
    aux RequestSeqGC{}              = "RequestSeqGC"
    aux RtsIdentifier{}             = "RtsIdentifier"
    aux RunThread{}                 = "RunThread"
    aux SendMessage{}               = "SendMessage"
    aux SendReceiveLocalMessage{}   = "SendReceiveLocalMessage"
    aux Shutdown{}                  = "Shutdown"
    aux SparkCounters{}             = "SparkCounters"
    aux SparkCreate{}               = "SparkCreate"
    aux SparkDud{}                  = "SparkDud"
    aux SparkFizzle{}               = "SparkFizzle"
    aux SparkGC{}                   = "SparkGC"
    aux SparkOverflow{}             = "SparkOverflow"
    aux SparkRun{}                  = "SparkRun"
    aux SparkSteal{}                = "SparkSteal"
    aux StartGC{}                   = "StartGC"
    aux Startup{}                   = "Startup"
    aux StopThread{}                = "StopThread"
    aux TaskCreate{}                = "TaskCreate"
    aux TaskDelete{}                = "TaskDelete"
    aux TaskMigrate{}               = "TaskMigrate"
    aux ThreadLabel{}               = "ThreadLabel"
    aux ThreadRunnable{}            = "ThreadRunnable"
    aux TickyBeginSample{}          = "TickyBeginSample"
    aux TickyCounterDef{}           = "TickyCounterDef"
    aux TickyCounterSample{}        = "TickyCounterSample"
    aux UnknownEvent{}              = "UnknownEvent"
    aux UserBinaryMessage{}         = "UserBinaryMessage"
    aux UserMarker{}                = "UserMarker"
    aux UserMessage{}               = "UserMessage"
    aux Version{}                   = "Version"
    aux WakeupThread{}              = "WakeupThread"
    aux WallClockTime{}             = "WallClockTime"

instance ApplyPadding (Decorated ds a)
      => ApplyPadding (Decorated ('(s, RawEventType) : ds) a) where
  computeTotalPadding padding _ = sum [
        fromMaybe 0 $ padRawEventType padding
      , computeTotalPadding padding (Proxy @(Decorated ds a))
      ]
  applyPadding padding totalPadding (DecorateWith typ a) = mconcat [
        padTo (padRawEventType padding) (getRawEventType typ)
      , applyPadding padding totalPadding a
      ]





