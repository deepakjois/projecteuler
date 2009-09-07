-- If the numbers 1 to 5 are written out in words: one, two, three, four,
-- five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?

import Data.Map

numbers ::  Integral a => [a]
numbers = [1..19] ++  [20,30..100] ++ [1000,1000000]
words_num = 
  [ "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen",
    "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety",
    "hundred",
    "thousand",
    "million"]

numwordsmap = fromAscList $ zipWith (,) numbers words_num

findNearest n =  last $ fst $ break (\x -> fst x > n) (toAscList numwordsmap)

convert n
      | n < 0  = "minus " ++ (convert (abs n))
      | n == 0 = "zero"
      | n > 0  = inwordsNearest ++ rest
                  where  (nnum,nwords) = findNearest n
                         inwordsNearest = if nnum < 100
                                            then nwords
                                            else (convert (n `div` nnum)) ++ " " ++ nwords
                         balance = n `mod` nnum
                         rest = if balance == 0
                                  then ""
                                  else " " ++ convert balance

main = do
  let countwords = length $ Prelude.filter (\x -> x /= ' ')  $ concat $ Prelude.map convert [1..1000]
      countands  = 3 * (length $ Prelude.filter (\x -> x `mod` 100 /= 0) [100..999])
  print $ countands + countwords