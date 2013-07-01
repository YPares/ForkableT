{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.ForkableT where

import Control.Monad.Cont
import Control.Applicative
import qualified Data.Traversable as T
import qualified Control.Concurrent.Async as C


newtype ForkableT r m a = ForkableT { forkableMToContT :: ContT [r] m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runForkableT :: (Applicative f) => ForkableT t f t -> f [t]
runForkableT (ForkableT act) = runContT act $ pure . (:[])

-- | A general function.
-- Extending it to work on every 'T.Traversable' is more difficult than
-- one would expect.
forkWith :: (Functor f) => ((a -> f [r]) -> [a] -> f [[r]]) -> [a] -> ForkableT r f a
forkWith mapper lst = ForkableT $ ContT $ \cont ->
  concat <$> mapper cont lst

forkA :: (Functor f, Applicative f) => [a] -> ForkableT r f a
forkA l = forkWith T.traverse l

-- | Just a shortcut for concurrent programming, as yo could really
-- use 'forkA' with Control.Concurrent.Async used as an Applicative.
-- Requires base monad to be IO. Could be abstracted to accept
-- instances of MonadIO instead (but would require monad-control or
-- similar, due to the fact that forkIO only accepts IO actions).
concFork :: [a] -> ForkableT r IO a
concFork = forkWith C.mapConcurrently

