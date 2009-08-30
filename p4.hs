-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91  99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

palindromic n = (show n) == (reverse (show n))

main = do
  let num =  (filter palindromic) $ [ x*y | x <- [999,998..100], y <- [999,998..100]  ]
  print $ maximum num
  