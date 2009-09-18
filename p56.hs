import Data.List
sumDigits n = sum $ unfoldr f n 
              where f 0 = Nothing
                    f x = Just (x `mod` 10, x `div` 10)

main = do
  print $ maximum $ map sumDigits  [a^b | a <- [1..100], b <- [1..100]]
