import qualified Data.Set as I
import Data.List
import Primes

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

pandigital = maximum $ filter isMillerRabinPrime $ map readInt $ permutations "1234567"


readInt :: String -> Integer
readInt s = read s

main = do
  print $ pandigital