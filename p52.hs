import qualified Data.Set as S

sameDigits x = (S.fromList (show x)) == (foldl1 S.union $ map (S.fromList.show)  $ zipWith (*) (repeat x) [2..6])

main = do
  print $ head $ dropWhile (not.sameDigits) [1..]