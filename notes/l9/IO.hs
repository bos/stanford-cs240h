instance Functor IO where
    fmap f act = do
      r <- act
      return (f r)
