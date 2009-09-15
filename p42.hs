import Text.Regex
import Data.List
import Data.Char

alphValueName n = sum $ map (\x -> ord x - 64) n

nameScore i n = i * alphValueName n

triangle = map (\x -> (x * (x+1)) `div` 2) [1..]

main = do
  namesStr <- readFile "words.txt"
  -- clumsy IO code
  let filteredList = filter (/= '"') namesStr
      names = sort $ splitRegex (mkRegex ",") $ filteredList
      scores = map alphValueName names
      maxScore = maximum scores
      triangleMax = takeWhile (<= maxScore) triangle
      triangleWords = filter (\x -> x `elem` triangleMax) scores
  print $ length $ triangleWords 
      
      