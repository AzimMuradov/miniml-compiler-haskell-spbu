module Main where

import Commands.Compile (compile)
import Commands.PrintCRuntime (printCRuntime)
import Commands.Run (run)
import Configuration.AppConfiguration (Command (..), Debug (Yes), MiniMl (MiniMl))
import Configuration.Commands.MiniMl (miniMl)
import Control.Monad (when)

-- * Main

main :: IO ()
main = do
  MiniMl cmd d <- miniMl

  when (d == Yes) $ do
    putStrLn $ "MiniML Configuration: " <> show cmd
    putStrLn ""

  case cmd of
    CmdRun r -> run r d
    CmdCompile c -> compile c d
    CmdPrintCRuntime pcr -> printCRuntime pcr
