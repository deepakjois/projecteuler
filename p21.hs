-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n
-- which divide evenly into n).
-- 
-- If d(a) = b and d(b) = a, where a b, then a and b are an amicable pair and
-- each of a and b are called amicable numbers.
-- 
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
-- and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71
-- and 142; so d(284) = 220.
-- 
-- Evaluate the sum of all the amicable numbers under 10000.

import qualified Data.Set as S
import Control.Arrow

sumdiv n = 1 + sumDivisors [2..sqrtN]
              where 
                sqrtN = (truncate $ sqrt $ fromIntegral n)
                sumDivisors  = foldl sumIfDivisor 0 
                sumIfDivisor a i = if n `mod` i == 0
                                      then
                                        if i*i == n then a+i else a + i + n `div` i
                                      else a

amicable = foldl addAmicable S.empty $ map (id &&& sumdiv)  [2..10000]
              where addAmicable a (i,s) = if (sumdiv s) == i && s /= i
                                            then S.insert i $ S.insert s a
                                            else a

main = do 
  print $ sum $ S.toList $ amicable                                            