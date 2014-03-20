import Control.Monad

listing:: [Int] -> Int -> [[Int]]
listing list n = if (n > 1) then return list ++ listing list (n - 1)
 		else return list

allCombinations :: [[Int]] -> [[Int]]
allCombinations [] = return []
allCombinations (l:ls) = l >>= \v -> allCombinations ls >>= \vs -> return (v : vs)

combo :: Int -> [[Int]]
combo n = allCombinations (listing [1, 2, 3] n)
