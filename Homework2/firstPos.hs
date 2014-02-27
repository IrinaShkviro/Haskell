firstPos1 :: Int -> [Int] -> Int -> Int
firstPos1 _ [] _ = 0
firstPos1 n (x : xs) c = if n == x 
	then c
	else firstPos1 n xs (c + 1)

firstPos :: Int -> [Int] -> Int
firstPos n s = firstPos1 n s 1