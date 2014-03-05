next :: Int -> Int
next 1 = 7
next 7 = 9
next 9 = 11
next x = if (r == 9) then (next s) * 10 + 1
	else s * 10 + next r where
	r = mod x 10
	s = div x 10

list179 = 1 : map next list179