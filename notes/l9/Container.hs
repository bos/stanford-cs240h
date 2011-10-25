newtype Container a = Container a

instance Functor Container where
    fmap f (Container a) = Container (f a)

class Funktor f where
    fkmap :: (a -> b) -> f a -> f b

instance Funktor ((->) a) where
    fkmap f g = \x -> f (g x)

instance Funktor [] where
    fkmap = map
