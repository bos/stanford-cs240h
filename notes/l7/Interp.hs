data Expr = List [Expr]
          | Num Int
          | Str String
          | Op BinOp Expr Expr
            deriving (Show)

data BinOp = Add | Concat
             deriving (Show)

interp x@(List _)                    = x
interp x@(Num _)                     = x
interp (Op Add (Num a) (Num b))      = Num (a + b)
interp (Op Concat (List a) (List b)) = List (a ++ b)
