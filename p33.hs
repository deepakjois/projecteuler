import Ratio
trySimplify (n,d) = notMul10 && (n1d1 || n2d1 || n1d2 || n2d2)
              where n1 = n `div` 10 
                    n2 = n `mod` 10
                    d1 = d `div` 10
                    d2 = d `mod` 10
                    notMul10 = n2 /= 0 && d2 /= 0
                    ratio = n % d
                    n1d1 = ratio == n1 % d1 && n2 == d2
                    n2d1 = ratio == n2 % d1 && n1 == d2
                    n1d2 = ratio == n1 % d2 && n2 == d1
                    n2d2 = ratio == n2 % d2 && n1 == d1

main = do
  print $ denominator $ foldl1 (*) $ map (\(a,b) -> a % b) $ filter trySimplify [ (a,b) | a <- [10..99], b <- [10..99], b > a ]