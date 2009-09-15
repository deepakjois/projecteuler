a n | n == 0 = 1
    | n == 1 = 40755
    | otherwise = 37634* (a $ n-1) - (a $ n-2) + 3136

main = do
  print $ a 2