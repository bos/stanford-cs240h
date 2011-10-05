module Canary where

import MergeSort
import Test.QuickCheck
import Data.Function (on)
import Data.List (sortBy)
import Data.Word (Word8)

p_stable xs = merged == sorted
  where merged = mergeSort (compare `on` fst) xs
        sorted = sortBy    (compare `on` fst) xs
        _witness = xs :: [(Word8, Word8)]
