import Control.Monad.ST
import Data.STRef

whee :: Int -> ST s Int
whee z = do
  r <- newSTRef z
  modifySTRef r (+1)
  readSTRef r
