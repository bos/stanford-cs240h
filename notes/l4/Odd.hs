import Test.QuickCheck
import Data.Word

p_sum_odd a b
    | odd a && odd b = even (a + b)
    | otherwise      = True

p_sum_odd1 a b = odd a && odd b ==> even (a+b)

p_b0rked a b = odd a && even a ==> even (a+b)

p_odd a b c = odd a && odd b && odd c ==> odd (a+b+c)

p_odd1 x (Odd a) (Odd b) (Odd c) = odd (a+b+c)
  where _witness = x == a

newtype Odd a = Odd a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Odd a) where
    arbitrary = do
      a <- arbitrary
      return $! Odd (if even a then a + 1 else a)
