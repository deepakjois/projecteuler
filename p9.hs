-- A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
-- a^2 + b^2 = c^2

-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

satisfy (a,b,c) = (a+b+c) == 1000

main = do
  print $ (take 1 (filter satisfy [  (a,b,c) | m <- [2,4..500], n <- [3,5..500], let a = (2*m*n), let b = abs(m^2-n^2), let c = (m^2 + n^2) ]))
                