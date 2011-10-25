oddEnough act = do
  v <- act
  if odd v
    then fail "too odd!"
    else return v
