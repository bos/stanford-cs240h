
import Control.Concurrent
import Control.Exception
import Control.Monad

type Mutex = MVar ThreadId

mutex_create :: IO Mutex
mutex_create = newEmptyMVar

mutex_lock, mutex_unlock :: Mutex -> IO ()
mutex_lock mv = myThreadId >>= putMVar mv
mutex_unlock mv = do mytid <- myThreadId
                     lockTid <- tryTakeMVar mv
                     unless (lockTid == Just mytid) $ error "mutex_unlock"

mutex_synchronize :: Mutex -> IO a -> IO a
mutex_synchronize mv action =
    bracket (mutex_lock mv) (\_ -> mutex_unlock mv)
                (\_ -> action)

tester mv = do
  tid <- myThreadId
  forever $ do
    mutex_synchronize mv $ putStrLn $ show tid ++ " got the lock"
    threadDelay 1000000

main :: IO ()
main = do
  mv <- mutex_create
  forkIO $ tester mv
  tester mv
