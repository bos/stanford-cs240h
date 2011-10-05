import Test.QuickCheck

data Point = Point Int Int

instance Arbitrary Point where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Point x y)
