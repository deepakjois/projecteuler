fac n = foldl1 (*) [1..n]

fac100 = 1 : scanl1 (*) [1..100]

exceedsMillion =  filter (>1000000) $ map nchooseR [(a,b) | a <- [1..100], b <- [1..100], a >= b]
                  where nchooseR (n,r) = (fac100 !! n)/( (fac100 !! r) * (fac100 !! (n-r)))

main = do
  print $ length $ exceedsMillion