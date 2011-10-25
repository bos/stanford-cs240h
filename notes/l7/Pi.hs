{-# LANGUAGE BangPatterns #-}

import System.Random
import Data.List (partition)
import Control.Arrow ((***))

montePi k g0 =
    fin . (length *** length) .
    partition (<=1) .
    take k . tail . map fst .
    iterate guess $ (undefined,g0)
  where
    fin (m,n) = 4 * fromIntegral m /
                fromIntegral (m+n)

guess :: (RandomGen g) => (Double,g) -> (Double,g)
guess (_,g) = (z,g'')
    where z = x^2 + y^2
          (x,g')  = random g
          (y,g'') = random g'

withGen :: (StdGen -> a) -> IO a
withGen f = do
  g <- getStdGen
  let (g',g'') = split g
  setStdGen g'
  return (f g'')
