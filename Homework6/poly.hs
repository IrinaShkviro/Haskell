data MT = Sum (Int, Int) MT | Null

showMT :: MT -> String
showMT Null = ""
showMT (Sum (k, d) f) = (show k) ++ "x^" ++ (show d) ++ "+" ++ showMT f

showP x = let
	mt = showMT x
	len = length mt
	in take (len - 1) mt

add :: MT -> MT -> MT
add x Null = x
add Null x = x
add (Sum (k1, d1) f1) (Sum (k2, d2) f2) =
	if (d1 > d2)
	then Sum (k1, d1) (add f1 (Sum (k2, d2) f2))
	else
		if (d2 > d1)
		then Sum (k2, d2) (add f2 (Sum (k1, d1) f1))
		else Sum (k1 + k2, d1) (add f1 f2)

mul :: MT -> MT -> MT -> MT
mul _ _ Null = Null
mul Null _ _ = Null
mul (Sum (k1, d1) f1) Null (Sum (k3, d3) f3) = 
	mul f1 (Sum (k3, d3) f3) (Sum (k3, d3) f3)
mul (Sum (k1, d1) f1) (Sum (k2, d2) f2) mt3  =
	Sum (k1 * k2, d1 + d2) (mul (Sum (k1, d1) f1) f2 mt3)

multiply :: MT -> MT -> MT
multiply x y = mul x y y

poly1 = Sum (2, 4) $ Sum (1, 3) $ Sum (5, 0) Null
poly2 = Sum (3, 3) $ Sum (4, 2) $ Sum (3, 0) Null