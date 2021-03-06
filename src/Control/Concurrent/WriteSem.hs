module Control.Concurrent.WriteSem
  ( WriteSem
  , newWriteSem
  , newWriteSemIO
  , withWriteSem ) where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Exception

newtype WriteSem = WriteSem (TVar Bool)

newWriteSem :: STM WriteSem
newWriteSem = WriteSem <$> newTVar True

newWriteSemIO :: IO WriteSem
newWriteSemIO = WriteSem <$> newTVarIO True

waitWriteSem :: WriteSem -> STM ()
waitWriteSem (WriteSem t) =
  readTVar t >>= \case
    True ->
      writeTVar t False
    False -> retry

signalWriteSem :: WriteSem -> STM ()
signalWriteSem (WriteSem t) =
  readTVar t >>= \case
    False ->
      writeTVar t True
    True -> retry

withWriteSem :: MonadIO m => WriteSem -> IO a -> m a
withWriteSem sem act = liftIO $
  bracket_
    (atomically $ waitWriteSem sem)
    (atomically $ signalWriteSem sem)
    act
