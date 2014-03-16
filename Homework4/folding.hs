data BinTree a = Empty | Node a (BinTree a) (BinTree a)

fold :: (a -> a -> a) -> a -> BinTree a -> a
fold f s Empty = s
fold f s (Node a left right) = fold f (fold f (f s a) right) left