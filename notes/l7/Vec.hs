{-# LANGUAGE BangPatterns #-}

module Vec (quicksort) where

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as U
import Data.List (partition)
import Control.Monad.ST

quicksort :: V.MVector s Int -> ST s ()
quicksort vec = go 0 (V.length vec-1)
  where
    go left right
      | left >= right = return ()
      | otherwise     = do
      idx <- partition left right
             ((left + right) `div` 2)
      go left (idx-1)
      go (idx+1) right

    partition left right pivotIdx = do
      pivot <- V.read vec pivotIdx
      V.swap vec pivotIdx right
      let loop i k
            | i == right = V.swap vec k right >>
                           return k
            | otherwise = do
            v <- V.read vec i
            if v < pivot
              then V.swap vec i k >> loop (i+1) (k+1)
              else loop (i+1) k
      loop left left

qsort (p:xs) = qsort lt ++ [p] ++ qsort ge
  where (lt,ge) = partition (<p) xs
qsort _      = []

vsort :: U.Vector Int -> U.Vector Int
vsort v = U.create $ do
            vec <- U.thaw v
            quicksort vec
            return vec
