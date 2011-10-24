import System.Random
import Stat

modify' :: MonadState s m => (s -> (a,s)) -> m a
modify' f = do
  s <- get
  let (a,s') = f s
  put s'
  return a

guess :: RandomGen g => State g Double
guess = do
  a <- modify' random
  b <- modify' random
  return (a*a + b*b)
