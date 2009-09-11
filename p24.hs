fac n = foldl1 (*) [1..n]

nearestFac x = head $ dropWhile (\n -> fac n <= x) [1..]

f n s | n == 0    = s
      | otherwise = (take (l-x) s) ++ rest
                    where l = length s
                          x = (nearestFac n)
                          rest = if x == l
                                   then 
                                      let idx =  n `quot` (fac (x - 1))
                                          lastChar   = s !! idx
                                          restString = filter (/= lastChar) s
                                          y = n - (idx * (fac (x-1)))
                                      in lastChar : (f y restString)
                                   else f n (drop (l-x) s)
