module Futures
(
  Future,
  fork,
  block,
)
where

import Futures.Prelude


{-|
Abstraction over a pattern where you delegate an IO action to be executed on another thread,
but are still interested in processing its result some time later on.
In the meantime you can execute other actions.
IOW, it is an implementation of asynchronous programming pattern.

Another way to look at future is as on an evaluate-once computation.
-}
newtype Future a = Future (IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

{-|
Fork a thread to execute the computation on and produce a future,
which will provide its results.

The IO action must not throw exceptions! If you want to transfer them, wrap it in 'try'.
-}
fork :: IO a -> IO (Future a)
fork io = do
  mv <- newEmptyMVar
  forkIO $ do
    a <- io
    putMVar mv a
  return $ Future $ readMVar mv

{-|
Block waiting until the future result is available.
-}
block :: Future a -> IO a
block (Future io) = io
