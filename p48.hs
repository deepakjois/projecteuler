main = do
  print $ foldl1 (+) $ map (\x -> x^x) [1..1000]