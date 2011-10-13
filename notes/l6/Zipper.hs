data Zipper a = Zipper [a] a [a]
              deriving (Show)

fromList :: [a] -> Zipper a
fromList [] = error "empty!"
fromList (x:xs) = Zipper [] x xs

next :: Zipper a -> Zipper a
next (Zipper ys y (x:xs)) = Zipper (y:ys) x xs
next z                    = z

prev :: Zipper a -> Zipper a
prev (Zipper (y:ys) x xs) = Zipper ys y (x:xs)
prev z                    = z

toList :: Zipper a -> [a]
toList (Zipper ys y xs) = reverse ys ++ y : xs
