

-- a^3 + b^3 = c^3 + d^3
-- a < c <= d < b

ramanujanNumbers :: [(Integer, Integer, Integer, Integer)]
ramanujanNumbers =
  [ (a, b, c, d)
  | b <- [1   .. ]
  , d <- [1   .. b -1]
  , c <- [1   .. d]
  , a <- [1   .. c -1]
  , a^3 + b^3 == c^3 + d^3
  ]


