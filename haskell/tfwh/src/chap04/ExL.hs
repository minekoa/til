{-# LANGUAGE FlexibleInstance #-}


module ExL where

import Prelude hidding (unzip)


fork :: (z -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)

unzip :: {(a, b)] -> ([a], [b])
unzip = fork (map fst, map snd)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = fork (f . fst, g . snd)

{-
1   cross (f, g) . fork (h, k) = fork (f . h, g . k)
2   fork (f, g) . h            = fork (f . g, g. h)
3   fst . cross (f, g) = f . fst
4   snd . cross (f, g) = g . snd
-}


{-
cross (f, g) . fork (h. fst, k . snd)
{- 1 -}
fork (f . h . fst), g . (k. snd))
{- . の結合速 -}

-}

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

type Pair = (,)

instance Bifunctor Pair where
  bimap = curry cross

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)

