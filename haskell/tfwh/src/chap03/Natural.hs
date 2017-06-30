
data Nat = Zero | Succ Nat

instance Eq Nat where
  Zero == Zero    = True
  Zero == Succ n  = False
  Succ m == Zero  = False
  Succ m == Succ n = (m == n)

instance Show Nat where
  show Zero            = "Zero"
  show (Succ Zero)     = "Succ Zero"
  show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"

