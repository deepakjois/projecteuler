import Data.List

-- | Generates all permutations of a multiset 
--   (based on \"algorithm L\" in Knuth; somewhat less efficient). 
--   The order is lexicographic.  
-- fasc2B_algorithm_L :: (Eq a, Ord a) => [a] -> [[a]] 
fasc2B_algorithm_L xs = unfoldr next (sort xs) -- (sort xs) where
  -- next :: [a] -> Maybe [a]
next xs = case findj (reverse xs,[]) of 
  Nothing -> Nothing
  Just ( (l:ls) , rs) -> Just $ (a,a)
                          where a = inc l ls (reverse rs,[]) 
  Just ( [] , _ ) -> error "permute: should not happen"
-- we use simple list zippers: (left,right)
-- findj :: ([a],[a]) -> Maybe ([a],[a])   
findj ( xxs@(x:xs) , yys@(y:_) ) = if x >= y 
  then findj ( xs , x : yys )
  else Just ( xxs , yys )
findj ( x:xs , [] ) = findj ( xs , [x] )  
findj ( [] , _ ) = Nothing

-- inc :: a -> [a] -> ([a],[a]) -> [a]
inc u us ( (x:xs) , yys ) = if u >= x
  then inc u us ( xs , x : yys ) 
  else reverse (x:us)  ++ reverse (u:yys) ++ xs
inc _ _ ( [] , _ ) = error "permute: should not happen"

