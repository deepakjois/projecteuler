sumAllDiagonals n = foldl (+) 1  [diagonalSum x | x <- [3,5..n]]


-- (n^2 - (n-1)^2)/4 = n-1
-- diagonals are (n^2), (n^2)-(n-1), (n^2)-2*(n-1), (n^2)-3(n-1)
-- summing and simplifying yields formula below
diagonalSum n = ((4*(n*n)) - (6 * (n-1)))

main = do
 print $ sumAllDiagonals 1001