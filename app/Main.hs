module Main (main) where

import Cmdline
import Command.ShowDelta (showDelta)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdCommand cmdline of
      ShowDelta -> showDelta cmdline
