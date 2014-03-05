sumList :: Num a => [a] -> [a]
sumList l = zipWith (+) (0 : l) (l ++ [0])

maxInSum :: Ord a => [a] -> a -> Int -> Int -> Int
maxInSum [] _ _ maxPos = maxPos
maxInSum (x : xs) mv cp mp = if (x > mv) then maxInSum xs x (cp + 1) cp
	else maxInSum xs mv (cp + 1) mp

firstPos :: Ord a => Num a => [a] -> Int
firstPos (x : xs) = maxInSum (sumList (x : xs)) x 1 1 - 1