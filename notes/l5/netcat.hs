
import Control.Concurrent
import Control.Exception
import Network.Socket
import System.IO

netcat :: String -> String -> IO ()
netcat host port = do
  -- Extract address from first AddrInfo in list
  AddrInfo{addrAddress = addr}:_
      <- getAddrInfo Nothing (Just host) (Just port)

  -- Create a TCP socket connected to server
  s <- socket AF_INET Stream 0
  connect s addr

  -- Convert socket to handle
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h NoBuffering  -- THIS IS IMPORTANT

  -- Deal w. broken unicode
  hSetBinaryMode stdout True

  -- Copy data back and forth
  done <- newEmptyMVar
  forkIO $ (hGetContents h >>= putStr) `finally` putMVar done ()
  getContents >>= hPutStr h
  takeMVar done


-- Example code
webServerAddr :: String -> IO SockAddr
webServerAddr name = do
  addrs <- getAddrInfo Nothing (Just name) (Just "www")
  return $ addrAddress $ head $ addrs


