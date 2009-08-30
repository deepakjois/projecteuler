-- The sum of the squares of the first ten natural numbers is,

-- 12 + 22 + ... + 102 = 385

--The square of the sum of the first ten natural numbers is,

--(1 + 2 + ... + 10)2 = 552 = 3025

-- Hence the difference between the sum of the squares of the first ten
-- natural numbers and the square of the sum is 3025  385 = 2640.

-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.

sum_of_squares l = sum (map (\s -> s*s) l)

square_of_sums l = (sum l) ^ 2

main = do
  let l = [1..100]
  print $ (square_of_sums l) - (sum_of_squares l)

