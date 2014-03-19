import Control.Monad

allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations fn [] = []
allCombinations fn (l:ls) = foldl (liftM2 fn) l ls

start :: IO()
start = do
	putStrLn("Give some value")
	readLn >>= \n -> print (allCombinations (*) [take n [1..], take n [1..]])