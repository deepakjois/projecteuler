import Text.Regex
import Data.List
import Data.Char

alphValueName n = sum $ map (\x -> ord x - 64) n

nameScore i n = i * alphValueName n

totalNameScore names = foldl addToNameScore (1,0) names
                        where addToNameScore (i,s) n = (i+1, s + nameScore i n)

main = do
  namesStr <- readFile "names.txt"
  -- clumsy IO code
  let filteredList = filter (/= '"') namesStr
      names = sort $ splitRegex (mkRegex ",") $ filteredList
  print $ totalNameScore names
      
      