data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)

instance Functor BinTree where
   fmap f (Leaf a) = Leaf (f a)
   fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Eq a => Eq (BinTree a) where
   Leaf a == Leaf b = (a == b)
   Branch l r == Branch l' r' = (l == l' && r == r')
   _ == _ = False

function n = if (n < 0) 
		then n
		else 0

findList list = filter (/= 0) (fmap (function) list)