data Tree a = Leaf a | Branch (Tree a) (Tree a)
minMax :: Tree a -> (Int , Int)
minMax (Leaf x) = (1 , 1)
minMax (Branch left right) = let
	maxh = if (fst (x1) > fst (x2)) 
		then fst (x1)
		else fst (x2)
	minh = if (snd (x1) < snd (x2))
		then snd (x1)
		else snd (x2)
	x1 = minMax left
	x2 = minMax right
	in (minh + 1, maxh + 1)