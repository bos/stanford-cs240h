import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map

type Address = String

data Number = N !(Map.Map Address Int) !Int
              deriving (Show)

renumber :: [(Address,Address)] -> [(Int,Int)]
renumber xs = evalState (mapM pair xs) (N Map.empty 0)
  where pair (x,y) = (,) <$> number x <*> number y

number :: Address -> State Number Int
number a = do
  N m i <- get
  case Map.lookup a m of
    Just j  -> return j
    Nothing -> do let i' = i + 1
                  put $! N (Map.insert a i m) i'
                  return i'
