data BinTree a = Empty | Node a (BinTree a) (BinTree a)

search :: (a -> Bool) -> BinTree a -> [a]
search c Empty = []
search c (Node a left right) = e ++ search c left ++ search c right 
	where e = if (c a) then [a]
		else []