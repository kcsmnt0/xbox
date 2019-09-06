module Xbox.Util where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Time.Clock.POSIX

next :: (Eq a, Enum a, Bounded a) => a -> a
next x = dropWhile (/= x) ([minBound..maxBound] ++ [minBound]) !! 1

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev x = dropWhile (/= x) (reverse [minBound..maxBound] ++ [maxBound]) !! 1

modifyT :: Monad m => (s -> m s) -> StateT s m ()
modifyT f = put =<< lift . f =<< get

milliTimestamp :: MonadIO m => m Int
milliTimestamp = round . ((10^6) *) <$> liftIO getPOSIXTime

debouncer :: MonadIO m => Int -> (a -> m b) -> m (a -> m (Maybe b))
debouncer t m = do
  v <- liftIO . newMVar =<< milliTimestamp
  return \x -> do
    t2 <- liftIO milliTimestamp
    liftIO (readMVar v) >>= \t1 ->
      if t2-t1 < t then
        return Nothing
      else do
        liftIO $ swapMVar v t2
        Just <$> m x

debouncer_ :: MonadIO m => Int -> (a -> m b) -> m (a -> m ())
debouncer_ t = fmap (void .) . debouncer t
