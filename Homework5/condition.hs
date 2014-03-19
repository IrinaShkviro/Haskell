condition1 :: Eq a => [a] -> (a -> Bool) -> Bool
condition1 list cond = list == (filter cond list)