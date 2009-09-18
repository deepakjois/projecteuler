fac n = foldl1 (*) [1..n]

fac100 = 1 : scanl1 (*) [1..100]

exceedsMillion =  filter (>1000000) $ map nchooseR [(n,r) | n <- [1..100], r <- [0..n]]
                  where nchooseR (n,r) = (fac100 !! n)/( (fac100 !! r) * (fac100 !! (n-r)))

main = do
  print $ length $ exceedsMillion