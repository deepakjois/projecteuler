-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.

-- What is the 10001st prime number?

isPrime :: Integral a => a -> Bool
isPrime p = p > 1 && (all (\n -> p `mod` n /= 0 ) $ takeWhile (\n -> n*n <= p) [2..])

main = do
  let p = (take 1).(drop 10000).(filter isPrime) $ [2..]
  print p