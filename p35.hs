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

rotateOnce :: Int -> Int
rotateOnce x = r * 10 ^ (numDigits - 1) + q
               where  (q,r) = x `divMod` 10
                      numDigits = length $ (show x) -- logBase 10 x is unreliable!

circPrimes = filter circular $ I.toList primeSet
             where
               primeSet = primes 1000000
               circular x = all (\p -> I.member p primeSet) $ take (length $ show x) $ iterate rotateOnce x 

main = do
  print $ length $ circPrimes
                      