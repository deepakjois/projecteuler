import Data.Char

main = do
 print $ sum $ map digitToInt $ show $ foldl (*) 1 [1..100]