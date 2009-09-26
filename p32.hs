import Data.List

pandigital (a,b,c) =  sort ((show a) ++ (show b) ++ (show c)) == "123456789"

-- Embarassingly Brute Force!
main = do
 print $ sum $ nub $ map (\(_,_,c) -> c) $ filter pandigital [(a,b,a*b) | a <- [1..50], b <- [1..2000]]

