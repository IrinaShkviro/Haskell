loop :: [Int] -> IO()
loop list = do
	putStrLn("Your command:")
	command  <- getLine
	case command of
		'0':_ -> return ()
		'1':_ -> do
			putStrLn("Value for add:")
			x <- readLn
			loop (addValue list x)
		'2':_ -> do
			putStrLn("Value for delete:")
			x <- readLn
			loop (removeValue list x)
		'3':_ -> do 
			putStrLn("Your list:")
			putStrLn (show list)
			loop list
		_ -> do putStrLn ("Incorrect value")
			loop list

start = do loop []

addValue :: [Int] -> Int -> [Int]
addValue [] x = [x]
addValue (l : ls) x = if (l < x)
	then (l : addValue ls x)
	else (x : l : ls)

removeValue :: [Int] -> Int -> [Int]
removeValue [] x = []
removeValue (l : ls) x = if (l < x)
	then (l : removeValue ls x)
	else if (l == x)
		then ls
		else (l : ls)