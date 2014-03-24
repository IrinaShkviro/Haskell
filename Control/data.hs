data Product = Book String String Int | Magazine String Int Int Int

coast (Book name author myCoast) = myCoast
coast (Magazine name year number myCoast) = myCoast

allCoast :: [Product] -> Int
allCoast list = foldr (+) 0 (map coast list)