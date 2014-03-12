corr :: [Char] -> [Char] -> Bool
corr [] [] = True
corr [] _ = False
corr (x : xs) l = case x of
	'(' -> corr xs (x : l)
	'[' -> corr xs (x : l)
	'{' -> corr xs (x : l)
	')' -> if ((l /= []) && (head l) == '(') then corr xs (tail l)
		else False
	']' -> if ((l /= []) && (head l) == '[') then corr xs (tail l)
		else False
	'}' -> if ((l /= []) && (head l) == '{') then corr xs (tail l)
		else False
	_ -> corr xs l

correct :: String -> Bool
correct x = corr x []