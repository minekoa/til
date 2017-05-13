import Data.Char (toLower, isAlpha)
import Data.Bool (bool)

palindrome :: IO ()
palindrome =
  do { putStrLn "Enter a string"
     ;  s <- getLine
     ;  putStrLn (if isPalindrome s then "Yes!" else "No!")
     }

isPalindrome :: String -> Bool
isPalindrome s =
    reverse s' == s'
  where
    s' = map toLower (filter isAlpha s)



palindrome' :: IO()
palindrome' = interact f
  where
    f =
      unlines
      . ("Enter a string:" :)
      . map (bool "No!" "Yes!" . isPalindrome)
      . lines
