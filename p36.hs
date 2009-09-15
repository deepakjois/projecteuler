import Data.List
import Data.Char
import Control.Arrow

toBase :: Int -> Int -> [Int]
toBase b v = toBase' [] v where
  toBase' a 0 = a
  toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b

fromBase :: Int -> [Int] -> Int
fromBase b ds = foldl' (\n k -> n * b + k) 0 ds

toAlphaDigits :: [Int] -> String
toAlphaDigits = map convert where
  convert n | n < 10    = chr (n + ord '0')
            | otherwise = chr (n + ord 'a' - 10)

fromAlphaDigits :: String -> [Int]
fromAlphaDigits = map convert where
 convert c | isDigit c = ord c - ord '0'
           | isUpper c = ord c - ord 'A' + 10
           | isLower c = ord c - ord 'a' + 10

palin x = x == (reverse x)

main = do
 print $ sum $ map fst $ filter (\(b10,b2) -> palin (show b10) && palin b2)  $ map (id &&& (toBase 2)) [1,3..999999] -- optimisation : check only for odd