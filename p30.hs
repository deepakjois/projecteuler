import Data.Char

result = sum $ filter (\x -> power5Digits x == x) [10..354294]
  where power5Digits = sum . map ((^5) . digitToInt) . show