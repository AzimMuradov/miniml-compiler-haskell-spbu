module Utils where

import Control.Monad (liftM2, liftM3)

liftM2' :: Monad m => (a' -> m a) -> (a -> a -> b) -> a' -> a' -> m b
liftM2' lifter f a b = liftM2 f (lifter a) (lifter b)

liftM3' :: Monad m => (a' -> m a) -> (a -> a -> a -> b) -> a' -> a' -> a' -> m b
liftM3' lifter f a b c = liftM3 f (lifter a) (lifter b) (lifter c)
