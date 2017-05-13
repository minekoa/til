
exp' :: Integer -> Integer -> Integer
exp' x n | n == 0 = 1
         | n == 1 = x
         | even n = r * r
         | odd n  = x * exp' x (n-1)
  where
    r = exp' x (n `div` 2)
    
