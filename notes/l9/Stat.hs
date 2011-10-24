{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}

module Stat where

class (Monad m) => MonadState s m where
    get :: m s
    put :: s -> m ()

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> let
        (a, s') = runState m s
        in runState (k a) s'

instance MonadState s (State s) where
    get   = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)
