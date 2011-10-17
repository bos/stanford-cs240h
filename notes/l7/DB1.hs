{-# LANGUAGE Rank2Types #-}

module DB1 where

import DB

newtype DB c a = DB {
      fromDB :: IO a
    }

instance Monad (DB c) where
    DB a >>= m = DB (a >>= \b -> fromDB (m b))
    return a   = DB (return a)
    fail s     = DB (fail s)

newtype SafeConn c = Safe Connection

withConnection :: Pool -> (forall c. SafeConn c -> DB c a) -> IO a
withConnection pool act =
  withConn pool $ \conn ->
    fromDB (act (Safe conn))
  
safeQuery :: SafeConn c -> String -> DB c [String]
safeQuery (Safe conn) str = DB (query conn str)

withConnectio :: Pool -> (forall c. ((->) (SafeConn c) (DB c a))) -> IO a
withConnectio = undefined
