import Data.List
 
pyramidalDiceSum =  map (\x -> (head x, length x)) $ group $ sort $ [d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 | d1 <- [1..4], d2 <- [1..4], d3 <- [1..4], d4 <- [1..4], d5 <- [1..4], d6 <- [1..4], d7 <- [1..4], d8 <- [1..4], d9 <- [1..4]]
cubicDiceSum =  map (\x -> (head x, length x)) $ group $ sort $ [d1 + d2 + d3 + d4 + d5 + d6 | d1 <- [1..6], d2 <- [1..6], d3 <- [1..6], d4 <- [1..6], d5 <- [1..6], d6 <- [1..6]]
 
calculateProbabilityDist tableSums = map (\x -> (fst x, (fromIntegral $ snd x)/(fromIntegral total))) tableSums
                                     where total = foldl (\a x -> a + (snd x))  0 tableSums
 
calculateFullProbablyDist = [ (p,c,mulProb) | x <- pDice, y <- cDice, let p = fst x, let c = fst y, let mulProb = (snd x) * (snd y) ]
                            where pDice = calculateProbabilityDist pyramidalDiceSum
                                  cDice = calculateProbabilityDist cubicDiceSum
 
findPWinningProbabilities = foldl  (\a (p,c,mulProb) -> if p > c then a+mulProb else a ) 0  calculateFullProbablyDist