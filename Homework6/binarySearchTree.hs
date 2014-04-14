data BinarySearchTree = Empty | Node Int BinarySearchTree BinarySearchTree

showTree :: BinarySearchTree -> String
showTree Empty = "Empty"
showTree (Node x left right) = "Node " ++ show x ++ " " ++ showTree left ++" " ++ showTree right

add :: Int -> BinarySearchTree -> BinarySearchTree
add x Empty = Node x Empty Empty
add x (Node v left right) = if (x > v)
				then Node v left $ add x right
				else Node v (add x left) right

help (Node x Empty r) startX = (Node startX Empty r, x)
help (Node x l r) startX = let 
				res = help l startX
				in (Node x (fst res) r, snd res)

midDel Empty Empty _ = Empty
midDel Empty x _ = x
midDel x Empty _ = x
midDel left (Node x2 left2 right2) forDel =
	let (tree, x) = help (Node x2 left2 right2) forDel
	in Node x left (del forDel tree)
	

del :: Int -> BinarySearchTree -> BinarySearchTree
del _ Empty = Empty
del x (Node v left right) = if (x > v)
				then Node v left (del x right)
				else
					if (x < v)
					then Node v (del x left) right
					else midDel left right v


search :: Int -> BinarySearchTree -> Bool
search _ Empty = False
search x (Node v left right) = if (x > v)
				then search x right
				else
					if (x < v)
					then search x left
					else True

height :: BinarySearchTree -> Int
height Empty = 0
height (Node v left right) =
	let 
		lh = height left
		rh = height right
		maxh = if (lh > rh)
			then lh
			else rh
	in maxh + 1

size' :: BinarySearchTree -> Int -> (Int, BinarySearchTree)
size' Empty st = (st, Empty)
size' (Node a left right) st =
	let (st', left') = size' left st
	    (st'', right') = size' right st'
	in (st'' + 1, Node a left' right')

size :: BinarySearchTree -> Int
size tree = fst $ size' tree 0


testTree =
        (Node 2
		(Node 1 Empty Empty) 
		(Node 4 
			(Node 3 Empty Empty) 
			(Node 5 Empty Empty)
		)
	)