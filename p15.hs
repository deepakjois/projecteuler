import Data.Map

-- fully recursive solution for 2x2 grid
numPaths2x2 c@(x,y)
      | c == (1,1) = 2
      | c == (2,1) = 1
      | c == (1,2) = 1
      | x > 2 || y > 2 = 0
      | otherwise  = numPaths (x+1, y) + numPaths (x, y+1)

-- fully recursive solution. Takes forever
numPaths c@(x,y) n
      | c == (n-1,n-1) = 2
      | c == (n,n-1) = 1
      | c == (n-1,n) = 1
      | x > n || y > n = 0
      | otherwise  = numPaths (x+1, y) n + numPaths (x, y+1) n

-- using Map to memoize results, and generating
-- co-ordinates in a predetermined order outwards
-- from the bottom right corner
numPaths_memoized n = seriesMap ! (0,0)
                      where 
                        seriesMap   = foldl calcFromMap (fromList []) [ (x,y) | m <- [n,n-1..0],
                                                                                x <- [n,n-1..m],
                                                                                y <- [n,n-1..m],
                                                                                x == m || y == m ]
                        calcFromMap m c@(x,y)
                                        | c == (n-1,n-1)   = insert c 2 m
                                        | c == (n,n-1)     = insert c 1 m
                                        | c == (n-1,n)     = insert c 1 m
                                        | c == (n,n)       = insert c 0 m
                                        | x+1 > n          = insert c (m!(x,y+1)) m -- x overflow, add down count only
                                        | y+1 > n          = insert c (m!(x+1,y)) m -- y overflow, add right count only
                                        | otherwise        = insert c sumPaths m    -- add right and down count
                                                              where
                                                                sumPaths = m!(x+1,y) + m!(x,y+1)
                                                
main = do
  print $ numPaths_memoized 20

