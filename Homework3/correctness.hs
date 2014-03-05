corr1 :: [Char] -> Int -> Bool
corr1 [] c = (c == 0)
corr1 (x : xs) c = if (x == '(') 
	then corr1 xs (c + 1)
	else if (x == ')')
		then if (c < 1)
			then False
			else corr1 xs (c - 1)
		else corr1 xs c

corr :: [Char] -> Bool
corr x = corr1 x 0