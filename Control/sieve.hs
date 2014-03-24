primes :: [Int]

primes = 2 : sieve [3, 5..]
	where
		sieve (x : xs) = x : sieve [k | k <- xs, mod k x /= 0]

test n = take n primes