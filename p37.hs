-- http://primes.utm.edu/glossary/page.php?sort=CircularPrime
import qualified Data.IntSet as I
 
-- findNext - finds the next member of an IntSet.
findNext c is | I.member c is = c
              | c > I.findMax is = error "Ooops. No next number in set."   
              | otherwise = findNext (c+1) is
 
-- mark - delete all multiples of n from n*n to the end of the set
mark n is = is I.\\ (I.fromAscList (takeWhile (<=end) (map (n*) [n..])))
                where
                    end = I.findMax is
 
-- primes - gives all primes up to n 
primes n = worker 2 (I.fromAscList [2..n])
                where
                    worker x is 
                     | (x*x) > n = is
                     | otherwise = worker (findNext (x+1) is) (mark x is)

truncateOnceLeft x = x `div` 10

truncateOnceRight x = x `mod` (10 ^ (numDigits - 1))
                      where numDigits = length $ (show x)


truncatablePrimes = filter truncatable $ I.toList primeSet
                    where
                      primeSet = primes 1000000
                      numDigits x = length $ show x 
                      truncateLeft x = take (numDigits x) $ iterate truncateOnceLeft x
                      truncateRight x = take (numDigits x) $ iterate truncateOnceRight x
                      truncatable x = all (\p -> I.member p primeSet) $ (truncateLeft x) ++ (truncateRight x)
main = do
  print $ sum $ drop 4 truncatablePrimes
                      