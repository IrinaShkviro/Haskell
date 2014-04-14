import Control.Monad.State

data Tree = Empty | Node String Tree Tree

treeStr :: Tree -> String
treeStr Empty = "e"
treeStr (Node x left right) = "n " ++ x ++ treeStr left ++ treeStr right

node (x : xs) st = let 
			(st', xs') = strTree xs st
			(st'', xs'') = strTree xs' st'
		   in ((Node [x] st' st''), xs'')

strTree :: String -> Tree -> (Tree, String)
strTree ('e' : xs) st = (Empty, xs)
strTree ('n' : ' ' : xs) st = node xs st

showTree :: Tree -> String
showTree Empty = "Empty"
showTree (Node x left right) = "Node " ++ x ++ " " ++ showTree left ++" " ++ showTree right

f str = showTree $fst $strTree str Empty

testTree =
        (Node "a"
		(Node "b" Empty Empty) 
		(Node "c" 
			(Node "d" Empty Empty) 
			(Node "e" Empty Empty)
		)
	)
testStr = "n an been cn deen eee"