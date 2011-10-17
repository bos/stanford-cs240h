module Interp2
    (
      Expr,
      num, str,
      add, cat,
      interp
    ) where

data Expr a = Num Int
            | Str String
            | Op BinOp (Expr a) (Expr a)
              deriving (Show)

data BinOp = Add | Concat
             deriving (Show)

interp :: Expr a -> Expr a
interp x@(Num _)       = x
interp x@(Str _)       = x
interp (Op Add a b)    = Num (i a + i b)
  where i x = case interp x of Num a -> a
interp (Op Concat a b) = Str (i a ++ i b)
  where i x = case interp x of Str y -> y

num :: Int -> Expr Int
num = Num

str :: String -> Expr String
str = Str

add :: Expr Int -> Expr Int -> Expr Int
add = Op Add

cat :: Expr String -> Expr String -> Expr String
cat = Op Concat
