{-# LANGUAGE NumericUnderscores #-}

module CodeGen.TimedValue where

import System.CPUTime (getCPUTime)

data TimedValue a = TimedValue
  { value :: a,
    time :: Nanoseconds
  }

-- | Nanoseconds, son!
newtype Nanoseconds = Nanoseconds Integer
  deriving (Show)

measureTimedValue :: IO a -> IO (TimedValue a)
measureTimedValue computation = do
  start <- getCPUTime
  val <- computation
  end <- getCPUTime
  let t = Nanoseconds $ (end - start) `div` 1_000
  return $ TimedValue val t

measureTime :: IO () -> IO Nanoseconds
measureTime computation = time <$> measureTimedValue computation
