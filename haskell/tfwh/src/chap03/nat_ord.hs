

data Nat
  = Zero
  | Succ Nat
  deriving Show

instance Eq Nat where
  Zero   == Zero   = True
  Zero   == Succ _ = False
  Succ _ == Zero   = False
  Succ n == Succ m = (n == m)

instance Ord Nat where
  Zero   <= Zero    = True
  Zero   <= Succ _  = True
  Succ _ <= Zero    = False
  Succ n <= Succ m  = (n < m)

instance Num Nat where
  m + Zero        = m
  m + Succ n      = Succ (m + n)

  m * Zero        = Zero
  m * Succ n      = m * n + m

  abs n           = n
  signum Zero     = Zero
  signum (Succ _) = Succ Zero

  m - Zero        = m
  Zero - Succ n   = Zero
  Succ m - Succ n = m - n


  fromInteger x
    | x <= 0    = Zero
    | otherwise = Succ (fromInteger (x -1))

divMod' :: Nat -> Nat -> (Nat, Nat)
divMod' m n
  | m < n   = (Zero, m)
  | otherwise = (Succ q, r)
  where
    (q, r) = divMod' (m - n) n


main = undefined
