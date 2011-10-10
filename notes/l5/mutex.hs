
import Control.Concurrent
import Control.Exception

type Mutex = MVar ()

mutex_create :: IO Mutex
mutex_create = newMVar ()

mutex_lock, mutex_unlock :: Mutex -> IO ()
mutex_lock = takeMVar
mutex_unlock mv = putMVar mv ()

mutex_synchronize :: Mutex -> IO a -> IO a
mutex_synchronize mv action =
    bracket (mutex_lock mv) (\_ -> mutex_unlock mv)
                (\_ -> action)


data Cond = Cond Mutex (MVar [MVar ()])

cond_create :: Mutex -> IO Cond
cond_create m = do
  waiters <- newMVar []
  return $ Cond m waiters

cond_wait, cond_signal, cond_broadcast :: Cond -> IO ()
cond_wait (Cond m waiters) = do
  me <- newEmptyMVar
  modifyMVar_ waiters $ \others -> return $ others ++ [me]
  mutex_unlock m
  takeMVar me `finally` mutex_lock m
  
cond_signal (Cond _ waiters) = modifyMVar_ waiters wakeone
    where wakeone [] = return []
          wakeone (w:ws) = putMVar w () >> return ws

cond_broadcast (Cond _ waiters) = modifyMVar_ waiters wakeall
    where wakeall ws = mapM_ (flip putMVar ()) ws >> return []
