-- import Data.List (head, tail)

data List a = Nil | Snoc (List a) a 

-- [1,2,3] =~= Snoc (Snoc (Snoc Nil 1) 2) 3)

shead :: List a -> a
shead (Snoc Nil x) = x
shead (Snoc xs x) = shead xs

slast :: List a -> a
slast (Snoc xs x) = x

toList :: [a] -> List a
toList = foldl Snoc Nil

--fromList :: List a -> [a]
--fromList (x : [])  = Snoc Nil x
--fromList (x : xs) = Snoc (toList xs) x



fromList :: List a -> [a]
fromList =
  reverse . unfoldr destruct
  where
    destruct (Snoc xs x) = Just (x, xs)
    destruct Nil         = Nothing



fromList' :: List a -> [a]
fromList' =
    rec' []
  where
    rec' a Nil = a
    rec' a (Snoc xs x) = rec' (x : a) xs


