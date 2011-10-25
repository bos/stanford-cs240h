module DB where

import Control.Exception

data Connection
data Transaction

begin :: Connection -> IO Transaction
begin conn = undefined

commit :: Transaction -> IO ()
commit txn = undefined

rollback :: Transaction -> IO ()
rollback txn = undefined

oops conn = do
  txn <- begin conn
  throwIO (AssertionFailed "the weebles!")
  commit txn

withTxn :: Connection -> IO a -> IO a
withTxn conn act = do
  txn <- begin conn
  r <- act `onException` rollback txn
  commit txn
  return r


query :: Connection -> String -> IO [String]
query = undefined

oops2 conn = withTxn

data Pool

getConn :: Pool -> IO Connection
getConn p = undefined

returnConn :: Pool -> Connection -> IO ()
returnConn p c = undefined

withConn :: Pool -> (Connection -> IO a) -> IO a
withConn pool act =
  bracket (getConn pool) (returnConn pool) act

evil pool = withConn pool return
