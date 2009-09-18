main = do
  -- embarassingly bruteforce
  print $  (28433 * 2 ^ 7830457 + 1) `mod` 10000000000 