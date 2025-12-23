module AOC.Util (pairs) where

-- | Creates a list of subsequent pairs
-- pairs [1,2,3] == [(1,2),(2,3),(3,4)]
pairs :: [a] -> [(a, a)]
pairs = zip <*> tail
