import Data.List
import qualified Data.Set as S

allAbundant n = filter abundant [1..n]
                  where abundant x = sumdiv x > x

sumdiv n = 1 + sumDivisors [2..sqrtN]
              where 
                sqrtN = (truncate $ sqrt $ fromIntegral n)
                sumDivisors  = foldl sumIfDivisor 0 
                sumIfDivisor a i = if n `mod` i == 0
                                      then
                                        if i*i == n then a+i else a + i + n `div` i
                                      else a

sum2Abundants = filter (not.test) range 
                 where !a  = allAbundant 20161
                       set = S.fromList a -- for efficient lookups
                       range = [1..46] ++ [47,49..20161] -- reducing problem space using Google ;)
                       test x =  any (\i -> (x-i) `S.member` set) a