module TestMergeSort where

import MergeSort
import Test.QuickCheck
import Data.List
import Data.Word

t_idempotent :: (Eq a) => (a -> a -> Ordering) -> [a] -> Bool
t_idempotent p = (mergeSort p . mergeSort p) === mergeSort p

t_commute p = mergeSort p === sortBy p

type C a = a -> a -> Ordering

(===) :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
(f === g) x = f x == g x

t_permute :: ([a] -> Bool) -> [a] -> Bool
t_permute prop = all prop . permutations

t_witnessed p a xs = mergeSort p (mergeSort p xs) == mergeSort p xs
  where _ = [a] == xs
