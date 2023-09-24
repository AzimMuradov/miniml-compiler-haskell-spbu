module Greeter where

hello :: String -> IO ()
hello arg = putStrLn $ "Hello, " <> arg <> "!"
