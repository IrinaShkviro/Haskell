reverseGradually :: [a] -> [a] -> [a]
reverseGradually s [] = s
reverseGradually s (x:xs) = reverseGradually (x:s) xs

myReverse :: [a] -> [a]
myReverse x = reverseGradually [] x