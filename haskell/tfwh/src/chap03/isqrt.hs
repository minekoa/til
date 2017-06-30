

-- 非負数の平方根の床を返す
-- 二乗して物の数を超えない数を二分探索で探す

isqrt :: Float -> Integer
isqrt x = fst (until unit (shrink x) (bound x))
  where
    unit (m, n) = m + 1 == n


type Interval = (Integer, Integer)

-- (1)ここの比較と
shrink :: Float -> Interval -> Interval
shrink x (m, n) = if (p^2) `leq` x then (p, n) else (m, p)
  where
    p = choose (m, n)


-- だいたい真ん中くらいをえらんでおけばいいでしょう。なので、
choose :: Interval -> Integer
choose (m, n) = (m + n) `div` 2


-- (3)bound で lowerが必要ないこと
bound :: Float -> Interval
bound x = (0, upper x)

-- (2)upperの作り方
upper :: Float -> Integer
upper x = until ((x `lt`) . (^2)) (* 2) 1


exp' :: Integer -> Integer -> Integer
exp' x n | n == 0 = 1
         | n == 1 = x
         | even n = r * r
         | odd n  = x * exp' x (n-1)
  where
    r = exp' x (n `div` 2)



leq :: Integer -> Float -> Bool
leq i f =
  (<=) (fromInteger i) f


lt :: Float -> Integer -> Bool
lt i f =
  (<) i (fromInteger f)





