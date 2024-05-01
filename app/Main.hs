module Main (main) where

import GhcEventsUtil.Cmdline
import GhcEventsUtil.Command.ShowDelta (showDelta)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdCommand cmdline of
      ShowDelta padding filters -> showDelta cmdline padding filters
