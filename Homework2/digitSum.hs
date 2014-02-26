digitSum :: Int -> Int
digitSum 0 = 0
digitSum n = digitSum (quot n 10) + rem n 10