
first p xs =
    head' . filter p
  where
    head' ys
      | null ys    = Nothing
      | otherwise = Just (head ys)

