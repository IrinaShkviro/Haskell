twoInPower :: Int -> Int
twoInPower 0 = 1
twoInPower n = 2 * twoInPower(n - 1)

deg :: Int -> [Int]
deg n = take n (map twoInPower [0..])