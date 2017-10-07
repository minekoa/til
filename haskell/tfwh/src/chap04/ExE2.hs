

triadd (x, y) = x^3 + y^3

sorted =
  [ [ (a, n -a) | a <- [1 .. n] ] | n <- [1..]]


triaddList n =
  [ (triadd t, t) | t <- concat $ tame n sorted ]




