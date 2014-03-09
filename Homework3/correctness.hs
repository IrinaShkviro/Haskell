corr1 :: [Char] -> Int -> Int -> Int -> Bool
corr1 [] c1 c2 c3 = (c1 == 0) && (c2 == 0) && (c3 == 0)
corr1 (x : xs) c1 c2 c3 = case x of
    '(' -> corr1 xs (c1 + 1) c2 c3
    ')' -> if (c1 < 1) then False
        else corr1 xs (c1 - 1) c2 c3
    '{' -> corr1 xs c1 (c2 + 1) c3
    '}' -> if (c2 < 1) then False
        else corr1 xs c1 (c2 - 1) c3
    '[' -> corr1 xs c1 c2 (c3 + 1)
    ']' -> if (c3 < 1) then False
        else corr1 xs c1 c2 (c3 - 1)
    _ -> corr1 xs c1 c2 c3

corr :: [Char] -> Bool
corr x = corr1 x 0 0 0