import Data.Char


cap :: String -> String
cap s
  | hd == "" = s
  | tl == "" = capW hd  -- hd /+ ""
  | otherwize = capW hd ++ "-" "" cap tl
  where
      (hd, tl') = break (== '-') s

      casW xxs = [tuUpper (head xxs)} ++ tail xss

capitalize :: String -> String
capitalize =
    unwords . map cap . words

