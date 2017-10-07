
--
-- 全部文字にして >> ワードに区切って
--                >> ワードでソートして
--                >> 続いている数を数えて (count, word)
--                >> 続いている数でソートして
--                >> take n して
--                >> 各行を文字列表示にして
--                >> くっつける
commonWords :: Int -> [Char] -> [Char]
commonWords n = concat . map showRun . take n
              . sortRuns . countRuns . sortWords
              . words . map toLower


showRun :: (Int, Word) -> [Char]
showRun (n, w) = w ++ ": " ++ show n ++ "\n"

countRuns :: [Word] -> [(Int, Word)]
countRuns [] = []
countRuns (w : ws) =
  (1 + length us, w) : countRuns vs
  where
    (us, vs) = span (== w) ws

sortWords :: [Word] -> [Word]
sortWords = sort

sortRuns :: [(Int,Word)] -> [(Int, Word)]
sortRuns = reverse . sort

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort ys) (sort zs)
  where
    (ys, zs) = halve xs

halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

merge [] ys = ys
merge xs [] = xs
merge (x: xs) (y:ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys



