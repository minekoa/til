
disjoint [] [] = False
disjoint [] _ = False
disjoint _ [] = False
disjoint (x:xs) (y:ys) 
  | x == y    = False
  | x < y     = disjoint xs (y:ys)
  | otherwise = disjoint (x:xs) ys



main :: IO ()
main = print $ disjoint [1,2,5] [2,4,6 :: Int]


