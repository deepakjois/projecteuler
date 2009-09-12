import Data.Char

d n | n < 10     = n
    | otherwise = digit
                  where
                    list       = takeWhile (< n) $ scanl1 (+)  $ map (\x -> 9 * (10^x) * (x+1)) [0..]
                    numDigits  = (length list) + 1
                    base       = last list
                    baseNum    = 10 ^ (numDigits - 1) - 1
                    (increment, digitPlace)  = (n - base) `quotRem` numDigits
                    number     = baseNum + increment
                    digit     = if digitPlace == 0
                                  then digitToInt $ last (show number)
                                  else digitToInt $ (show (number + 1)) !! (digitPlace - 1)
