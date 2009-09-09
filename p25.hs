let fibs = 1 : 1 :  zipWith (+) fibs (tail fibs)

main = do
  print $ 1 + (  length $ takeWhile (\x -> length (show x) < 1000) fibs )