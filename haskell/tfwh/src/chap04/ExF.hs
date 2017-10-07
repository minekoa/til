

data List a = Nil | Snoc (List a) a

shead :: List a -> a
shead (Snoc Nil x) = x
shead (Snoc xs x) = shead xs

slast :: List a -> a
slast (Snoc xs x) = x

