diff :: Eq a => [a] -> Bool
diff [] = True
diff (x : xs) = if (elem x xs)
	then False
	else diff xs