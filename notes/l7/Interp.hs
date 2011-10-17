data Expr = Num Int             -- atom
          | Str String          -- atom
          | Op BinOp Expr Expr  -- compound
            deriving (Show)

data BinOp = Add | Concat
             deriving (Show)

interp x@(Num _)                     = x
interp x@(Str _)                     = x
interp (Op Add a b)      = Num (i a + i b)
  where i x = case interp x of Num a -> a
interp (Op Concat (Str a) (Str b))   = Str (a ++ b)
