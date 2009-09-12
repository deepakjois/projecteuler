import Data.List
import Data.Function
import Control.Arrow

recurringCycle n = longdiviter 1 n []

longdiviter dividend divisor qrList
                     | dividend == 0 = 0
                     | dividend < divisor = longdiviter (dividend*10) divisor ((0,dividend):qrList)
                     | otherwise = if (q,r) `elem` qrList
                                     then 1 + (length $ takeWhile (/= (q,r)) qrList)
                                     else longdiviter (r*10) divisor ((q,r):qrList)
                                   where 
                                     (q,r) = dividend `quotRem` divisor

main = do
  print $ maximumBy (compare `on` snd) $ map (id &&& recurringCycle) [2..1000]
                                    