
mkYS :: Integer -> [Integer]
mkYS x
  | odd x     = [1..]
  | otherwise = [0]

