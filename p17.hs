-- If the numbers 1 to 5 are written out in words: one, two, three, four,
-- five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?

import Data.Map

numbers ::  Integral a => [a]
numbers = [1..19] ++  [20,30..100] ++ [1000]
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
    "thousand"]

numwordsmap = fromList $ zipWith (,) numbers words_num

convert n
      | n == 0               = ""
      | n < 21               = numwordsmap ! n
      | n >= 21 && n < 100   = let mul10 = numwordsmap ! (n - n `mod` 10)
                                   rem10 = convert (n `mod` 10)
                               in mul10 ++ " " ++ rem10
      | n >= 100 && n < 1000 = let mul100 = convert (n `div` 100) ++ " " ++ (numwordsmap ! 100)
                                   rem100 = if (n `mod` 100) > 0 
                                              then " and " ++ (convert (n `mod` 100))
                                              else ""
                               in mul100 ++ rem100
      | n == 1000            = "one thousand"