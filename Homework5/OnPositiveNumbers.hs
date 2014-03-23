import Control.Monad

decompos :: Int -> Int -> [[Int]]

decompos 0 _ = [[]]
decompos n start = [start..n] >>= (\x -> ((map (x :) (decompos (n - x) x))))

dec :: Int -> [[Int]]
dec n = decompos n 1