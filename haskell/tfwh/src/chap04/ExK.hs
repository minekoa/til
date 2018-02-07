module ExK where

fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)

unzip :: [(a, b)] -> ([a], [b])
unzip = fork (map fst, map snd)

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = fork (f . fst, g . snd)


{-
1   cross (f, g) . fork (h, k) = fork (f . h, g . k)
2   fork (f, g) . h            = fork (f . g, g. h)
3   fst . cross (f, g) = f . fst
4   snd . cross (f, g) = g . snd
-}

-- cross (map f, map g) . unzip = unzip . map (cross (f, g))　の証明


{-
cross (map f, map g) . unzip = unzip . map (cross (f, g))
{- unzip の展開 -}
cross (map f, fap g) . fork (map fst, map snd)
{- 1 -}
fork (map f . map fst, map g . map snd)
{- map の function則 -}
fork (map (f . fst), map (g . snd))
{- 3 と 4 -}
fork (map（fst . cross(f，g))，map(snd . map(cross（f，g)))
{- map の founctor則 展開 -}
fork (map fst . map (cross (f, g)), map snd . map (cross (f, g)))
{- 2 -}
fork (map fst, map snd) . map (cross (f, g))
{- unzip -}
unzip .map (cross (f, g))
-}

