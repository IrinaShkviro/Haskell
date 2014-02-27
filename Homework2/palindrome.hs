withoutSpaces :: [Char] -> [Char]
withoutSpaces [] = []
withoutSpaces (s : xs) = if (s == ' ') 
	then withoutSpaces xs
	else s : withoutSpaces xs

isPalindrome :: [Char] -> Bool
isPalindrome s = (reverse t == t) 
	where t = withoutSpaces s 