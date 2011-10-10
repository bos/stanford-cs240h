
module Main where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Criterion.Main

pingpong :: Bool -> Int -> IO ()
pingpong v n = do
  mvc <- newEmptyMVar
  mvp <- newEmptyMVar
  let parent n | n > 0 = do when v $ putStr $ " " ++ show n
                            putMVar mvc n
                            takeMVar mvp >>= parent
             | otherwise = return ()
      child = do n <- takeMVar mvc
                 putMVar mvp (n - 1)
                 child
  tid <- forkIO child
  parent n `finally` killThread tid
  when v $ putStrLn ""

main :: IO ()
main = defaultMain [
        bench "thread switch test" mybench
       ]
    where mybench = pingpong False 10000





wrap :: IO a -> IO a
wrap action = do
  mv <- newEmptyMVar
  _ <- forkIO $ (action >>= putMVar mv) `catch`
       \e@(SomeException _) -> putMVar mv (throw e)
  takeMVar mv

wrap' :: IO a -> IO a
wrap' action = do
  mv <- newEmptyMVar
  mask $ \unmask -> do
      tid <- forkIO $ (unmask $ action >>= putMVar mv) `catch`
             \e@(SomeException _) -> putMVar mv (throw e)
      let loop = takeMVar mv `catch` \e@(SomeException _) ->
                 throwTo tid e >> loop
      loop


