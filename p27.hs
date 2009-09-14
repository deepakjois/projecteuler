import Data.List
import Data.Function

isPrime p = p > 1 && (all (\n -> p `mod` n /= 0 ) $ takeWhile (\n -> n*n <= p) [2..])

numPrimes a b = length $ takeWhile isPrime [ n^2 + a*n + b | n <- [0..] ]

-- Brute Force (takes forever)
result = maximumBy (compare `on` snd) $ [ ((a,b),(numPrimes a b))| a <- [-1000..1000], b <- [-1000..1000] ]

-- n^2-79n+1601 =  (n-40)^2+n-40+41
-- (n-p)^2+(n-p)+41
-- n^2-(1-2t)+(t^2-t+41)
--  |1-2t| < 1000 and |t^2-t+41| < 1000
--  -30 <= t <= 31
result1 = maximumBy (compare `on` snd) $ [ ((a,b),(numPrimes a b)) | t <- [-30..31], let a = (1-2*t), let b = (t^2-t+41) ]

main = do
 let (a,b) =  fst result1
 print $ a*b

    
