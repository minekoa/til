
floor' :: Float -> Integer
floor' = read . takeWhile (/= '.') . show


