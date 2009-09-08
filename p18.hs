-- By starting at the top of the triangle below and moving to adjacent numbers
-- on the row below, the maximum total from top to bottom is 23.
--  
-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3
--  
-- That is, 3 + 7 + 4 + 9 = 23.
--  
-- Find the maximum total from top to bottom of the triangle below:
--  
-- 75
-- 95 64
-- 17 47 82
-- 18 35 87 10
-- 20 04 82 47 65
-- 19 01 23 75 03 34
-- 88 02 77 73 07 63 67
-- 99 65 04 28 06 16 70 92
-- 41 41 26 56 83 40 80 70 33
-- 41 48 72 33 47 32 37 16 94 29
-- 53 71 44 65 25 43 91 52 97 51 14
-- 70 11 33 28 77 73 17 78 39 68 17 57
-- 91 71 52 38 17 14 91 43 58 50 27 29 48
-- 63 66 04 68 89 53 67 30 73 16 69 87 40 31
-- 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

import Data.Map
import qualified Data.List as L
import System.IO

triangle4 = 
  [
    [3],
    [7, 4],
    [2, 4, 6],
    [8, 5, 9, 3]
  ]

triangle5 =
  [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10]    
  ]

triangle15 = 
  [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
  ]


neighbors c@(x,y) n = [(x',y+1) | x' <- [x..x+1], x <= (y+1), x'<n && (y+1)<n ]

genAllTriangleNodes n  = [(x,y) | y <- [0..n-1], x <- [0..y]]

initDistMap n = fromList $ L.map (\x -> (x,0)) $ genAllTriangleNodes n


initPrevMap n = fromList $ L.map (\x -> (x,(0,0))) $ genAllTriangleNodes n

distance t c@(x,y) 
              | y >= (L.length t) = 0
              | otherwise       = (t !! y !! x)

maxPath triangle = maxPathIter triangle distMap prevMap nodes
                    where
                      distMap = insert (0,0) (distance triangle (0,0)) (initDistMap (L.length triangle))
                      prevMap = initPrevMap (L.length triangle)
                      nodes   = (genAllTriangleNodes (L.length triangle))

maxPathIter triangle distMap prevMap remainingNodes
                            | (L.null remainingNodes) = distMap 
                            | otherwise = maxPathIter triangle newDistMap newPrevMap  newRemainingNodes
                                             where
                                                currNode = L.maximumBy (\a b -> compare  (distMap ! a) (distMap ! b)) remainingNodes
                                                totalDistance c@(x,y) = (distMap ! currNode) + (distance triangle c)
                                                
                                                updateDistMap m node = if (m ! node) < (totalDistance node)
                                                                        then insert node (totalDistance node) m
                                                                        else m

                                                updatePrevMap m node = if (distMap ! node) < (totalDistance node)
                                                                         then  insert node currNode m
                                                                         else  m
                                                newDistMap  = L.foldl' updateDistMap distMap (L.filter (\x ->  L.elem x remainingNodes) $ neighbors currNode (length triangle))
                                                newPrevMap  = L.foldl' updatePrevMap prevMap (L.filter (\x ->  L.elem x remainingNodes) $ neighbors currNode (length triangle))
                                                newRemainingNodes = L.filter (/= currNode) remainingNodes


path (0,0) prevMap = [(0,0)]
path p prevMap = p : path (prevMap ! p) prevMap

build = L.map (\x -> distance triangle15 x) 