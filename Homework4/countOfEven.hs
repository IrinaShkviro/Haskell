count1 :: [Int] -> Int
count1 x = length (filter even x)

count2 :: [Int] -> Int
count2 x = length (filter (> 0) (map f x))

f :: Int -> Int
f x = if (mod x 2 == 0)
	then 1
	else 0

count3 :: [Int] -> Int
count3 x = foldr (+) 0 (map f x)