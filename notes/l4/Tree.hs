import Test.QuickCheck
import Control.Monad

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [
                  Leaf `liftM` arbitrary
                , liftM2 Node arbitrary arbitrary
                ]
