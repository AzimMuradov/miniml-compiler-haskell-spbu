module Main where

import Commands.Compile (compile)
import Commands.Run (run)
import Commands.Verify (verify)
import Configuration.AppConfiguration (Command (..), Debug (Yes), MiniMl (MiniMl))
import Configuration.Commands.MiniMl (miniml)
import Control.Monad (when)

-- * Main

main :: IO ()
main = do
  MiniMl cmd d <- miniml

  when (d == Yes) $ do
    putStrLn $ "MiniML Configuration: " <> show cmd
    putStrLn ""

  case cmd of
    CmdCompile c -> compile c d
    CmdRun r -> run r d
    CmdVerify v -> verify v d
