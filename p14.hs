-- The following iterative sequence is defined for the set of positive
-- integers:
--  
-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)
--  
-- Using the rule above and starting with 13, we generate the following
-- sequence: 13  40  20  10  5  16  8  4  2  1
--  
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem),
-- it is thought that all starting numbers finish at 1.
--  
-- Which starting number, under one million, produces the longest chain?
--  
-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Data.Array
import Data.List
import qualified Data.Set as S

collatz n | even n = n `div` 2
          | odd  n = 3*n + 1
          

-- original solution (takes forever for 999999)
collatzSequence n
              | n == 1    = [1]
              | otherwise = n:(collatzSequence (collatz n))


maxStepsCollatz n = maxStepsCollatz' 2 n (S.fromAscList [2..n]) 1 1
                     where 
                       maxStepsCollatz' curr limit remSet maxCurr maxCurrSteps
                                                                       | curr > limit = (maxCurr,maxCurrSteps)
                                                                       | S.notMember curr remSet = maxStepsCollatz' (curr+1) limit remSet maxCurr maxCurrSteps
                                                                       | otherwise    = maxStepsCollatz' (curr+1) limit remSetNew maxNew maxStepsNew
                                                                                         where
                                                                                           cs  = collatzSequence curr
                                                                                           lcs = (length cs)
                                                                                           remSetNew = foldl' (\s i -> S.delete i s) remSet cs
                                                                                           (maxNew,maxStepsNew) = if lcs > maxCurrSteps
                                                                                                                    then (curr,lcs)
                                                                                                                    else (maxCurr, maxCurrSteps)
-- solution adapted from a forum soln
numStepsSeries n = seriesArray
                  where
                    seriesArray = listArray (1,n) $ 0:[numSteps x x | x <- [2..n] ]
                    numSteps x i  = if (collatz x) <= i
                                    then 1 + (seriesArray ! (collatz x))
                                    else 1 + numSteps (collatz x) i


main = 
    print $ foldl' maxBySteps (1,0) $ assocs $ numStepsSeries 1000000
    where
     maxBySteps x@(_,a) y@(_,b) = if a > b then x else y