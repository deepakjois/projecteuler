import qualified Data.Map as M
import Data.List


discovery names = foldl mapCode M.empty names
                  
mapCode :: M.Map  Char (String, String) -> [Char] -> M.Map Char (String,String)
mapCode map [n1,n2,n3] = newN3entry $ newN2entry $ newN1entry map
    where (n1L,n1R)  = M.findWithDefault ("","") n1 map
          (n2L,n2R)  = M.findWithDefault ("","") n2 map
          (n3L,n3R)  = M.findWithDefault ("","") n3 map
          newN1entry m = M.insert n1  (n1L, nub $ n2:n3:n1R) m
          newN2entry m = M.insert n2 (nub $ n1:n2L,nub $ n3:n2R) m
          newN3entry m = M.insert n3 (nub $ n1:n2:n3L,n3R) m

calculateCode map = unfoldr f map 
                where
                  f b = let nextCode = head $ M.keys $ M.filter (\(l,_) -> l == "") b
                            lF x l = filter (/= x) l
                            nextB = M.map (\(l,r) -> (lF nextCode l, lF nextCode r) ) b
                        in
                          Just (nextCode, M.delete nextCode nextB)
               

main = do
  namesStr <- readFile "keylog.txt"
  let names = map (take 3) $ lines namesStr
  print $ discovery names
  print $ take 6 $ calculateCode $ discovery names
      

-- Interesting Soln in forums
{-
module Main where
 
import Data.Char (digitToInt, intToDigit)
import Data.Graph (buildG, topSort)
import Data.List (intersect)
import System.IO.Unsafe (unsafePerformIO)
 
data_p79 :: String
data_p79 = unsafePerformIO . readFile $ "Data/p79.txt"
 
p79 :: Int
p79 = read . intersect graphWalk $ usedDigits
  where
    usedDigits = intersect "0123456789" $ data_p79
    edges = concat . map (edgePair . map digitToInt) . words $ data_p79
    graphWalk = map intToDigit . topSort . buildG (0, 9) $ edges
    edgePair [x, y, z] = [(x, y), (y, z)]
    edgePair _         = undefined
 
prop_p79 :: Bool
prop_p79 = p79 == 73162890
 
main :: IO ()
main = print p79

-}
