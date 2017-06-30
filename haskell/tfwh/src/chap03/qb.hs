

(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^  y = 
  if  y >= 0 then  x ^ y
  else recip $ x ^ (negate y)




