module MergeSort (mergeSort) where

mergeSort :: (a -> a -> Ordering) -> [a] -> [a]
mergeSort pred = go
  where
    go []  = []
    go [x] = [x]
    go xs  = merge (go xs1) (go xs2)
      where (xs1,xs2) = split xs

    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys) =
      case pred x y of
        GT -> y : merge (x:xs) ys
        _  -> x : merge xs (y:ys)

split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:xs,y:ys)
  where (xs,ys) = split zs
