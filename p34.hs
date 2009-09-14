fac n | n == 0    = 1 
      | otherwise = foldl1 (*) [1..n]

sumOfDigitFactorial n | n == 0 = 1
                      | n < 10 = (fac n)
                      | otherwise = fac (n `mod` 10) + sumOfDigitFactorial (n `quot` 10)