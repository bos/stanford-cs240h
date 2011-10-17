module Ref
    (
      Ref, newRef, readOnly,
      readRef, writeRef
    ) where

import Data.IORef

newtype Ref t a = Ref (IORef a)

data ReadOnly
data ReadWrite

newRef :: a -> IO (Ref ReadWrite a)
newRef a = Ref `fmap` newIORef a

readRef :: Ref t a -> IO a
readRef (Ref ref) = readIORef ref

writeRef :: Ref ReadWrite a -> a -> IO ()
writeRef (Ref ref) v = writeIORef ref v

readOnly :: Ref t a -> Ref ReadOnly a
readOnly (Ref ref) = Ref ref
