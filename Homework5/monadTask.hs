import Control.Monad

test :: [Int] -> Int -> Bool
test list  x = ((list !! x > list !! (x - 1)) && (list !! x > list !! (x + 1)))

first list = do
		when (length list > 2) 
			(do
				when (test list 2) (first (tail list))
				liftM print list !! 1)