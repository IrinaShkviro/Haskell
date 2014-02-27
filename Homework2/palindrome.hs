withoutSpaces :: [Char] -> [Char]
withoutSpaces [] = []
withoutSpaces (s : xs) = if (s == ' ') 
	then withoutSpaces xs
	else s : withoutSpaces xs

isPalindrome :: [Char] -> Bool
isPalindrome s = if (reverse t == t) 
	then True
	else False
	where t = withoutSpaces s 