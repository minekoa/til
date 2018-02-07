
-- cp : cartesian product : デカルト積

cp (xs : xss) = [ x : ys | x <- xs , ys <- cp xss ]

cp (xs : xss) = do
  x <- xs
  ys <- cp xss
  return $ x : ys

cp (xs : xss) =
  xs     >>= \x ->
  cp xss >>= \ys ->
  return( x : ys)

cp (xs : xss) =
  concatMap (\x ->
   cp xss >>= \ys ->
   [x : ys] )
  xs


cp (xs : xss) =
  concatMap
  (\x -> concatMap (\ys -> [x : ys]) (cp xss) xs

-- 外側のconcatMapがダメ


concatMap f yss = cncast (map f yss)
  
