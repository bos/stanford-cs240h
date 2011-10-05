module TestMergeSort where
import MergeSort
import Test.QuickCheck

t_idempotent :: (Eq a) => (a -> a -> Bool) -> [a] -> Bool
t_idempotent p xs = mergeSort p ys == ys
  where ys = mergeSort p xs
