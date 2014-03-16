loop :: Ord a => [a] -> IO()
loop a = do
	command  <- getLine
	case command of
		'0':_ -> return ()
		'1':x -> loop (addValue a x)
		'2':x -> loop (removeValue a x)
		'3':_ -> do show a
			loop a
		_ -> do putStrLn ("Incorrect value")
			loop a

start = do loop []

addValue :: Ord a => [a] -> a -> [a]
addValue [] x = [x]
addValue (l : ls) x = if (l < x)
	then (l : addValue ls x)
	else (x : l : ls)

removeValue :: Ord a => [a] -> a -> [a]
removeValue [] x = []
removeValue (l : ls) x = if (l < x)
	then (l : removeValue ls x)
	else if (l == x)
		then ls
		else (l : ls)