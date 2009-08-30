-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.


multiple_of_three_or_five n = (n `mod` 3 == 0) || (n `mod` 5 == 0) 

main = do
  let nos = (filter multiple_of_three_or_five) [1..999]
  print $ sum nos