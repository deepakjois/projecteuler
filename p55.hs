reverseAndAdd :: Integer -> Integer
reverseAndAdd x = x + reverseNum
    where reverseNum = read $ (reverse $ show x)

isNotLychreal x = any isPalindrome $ take 50 $ drop 1 $ iterate reverseAndAdd x 
               where isPalindrome y = (show y) == (reverse $ show y)

main = do
  print $ length $ filter (not . isNotLychreal) [1..10000]

