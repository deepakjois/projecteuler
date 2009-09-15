-- http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples
import Data.List
import Data.Function
 
-- primitive generator
gen3 (a,b,c) = takeWhile (\(x,y,z) -> x+y+z < 1000) $ tA:tB:tC:rest
               where
                 tA = ((a - 2*b + 2*c),(2*a - b + 2*c),(2*a - 2*b + 3*c)) 
                 tB = ((a + 2*b + 2*c),(2*a + b + 2*c),(2*a + 2*b + 3*c))
                 tC = ((2*b + 2*c - a),(b + 2*c - 2*a),(2*b + 3*c - 2*a))
                 rest = (gen3 tA) ++ (gen3 tB) ++ (gen3 tC)

-- non primitive generator (by using a multiple)
genNonPrimitive t = takeWhile (\(x,y,z) -> x+y+z < 1000) $ zipWith (\n (x,y,z) -> (x*n,y*n,z*n)) [1..] (repeat t)

main = do
 print $  head $ maximumBy (compare `on` length) $ group $ sort $ map (\(a,b,c) -> a+b+c)  $ concatMap genNonPrimitive $ gen3 (3,4,5)
