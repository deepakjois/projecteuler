import qualified Data.Set as S

distinct = foldl (\a x -> S.insert  x a) S.empty  [ x^y | x <- [2..100], y <- [2..100]]

main = do
  print $ length $ S.toList distinct