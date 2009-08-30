-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the 600851475143 ?


-- Haskell Wiki : http://www.haskell.org/haskellwiki/99_questions/31_to_41
primeFactors n = primeFactors' n 2 where
    primeFactors' n factor
      | factor*factor > n   = [n]
      | n `mod` factor == 0 = factor : primeFactors' (n `div` factor) factor
      | otherwise           = primeFactors' n (factor + 1)

main = do
  print $ maximum (primeFactors 600851475143)