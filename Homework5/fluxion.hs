data FluxType a = Degree (FluxType a) Int
	| Sum (FluxType a) (FluxType a)
	| Divis (FluxType a) (FluxType a)
	| Mul (FluxType a) (FluxType a)
	| Sub (FluxType a) (FluxType a)
	| Const a
	| Var

simplify (Degree var 1) = var
simplify (Degree var 0) = Const 1
simplify (Sum (Const 0) arg2) = arg2
simplify (Sum arg1 (Const 0)) = arg1
simplify (Divis arg1 (Const 1)) = arg1
simplify (Divis (Const 0) _) = Const 0
simplify (Mul (Const 1) arg2) = arg2
simplify (Mul arg1 (Const 1)) = arg1
simplify (Mul (Const 0) _) = Const 0
simplify (Mul _ (Const 0)) = Const 0
simplify (Sub arg1 (Const 0)) = arg1
simplify (Degree arg1 arg2) = Degree arg1 arg2
simplify (Sum arg1 arg2) = Sum (simplify arg1) (simplify arg2)
simplify (Divis arg1 arg2) = Divis (simplify arg1) (simplify arg2)
simplify (Mul arg1 arg2) = Mul (simplify arg1) (simplify arg2)
simplify (Sub arg1 arg2) = Sub (simplify arg1) (simplify arg2)
simplify (Const a) = Const a
simplify Var = Var

p' (Degree var deg) = (Mul (Mul (Const deg) (Degree var (deg - 1))) (myP var))
p' (Sum ad1 ad2) = (Sum (myP ad1) (myP ad2))
p' (Divis arg1 arg2) = (Divis (Sub (Mul (myP arg1) arg2) (Mul (myP arg2) arg1)) (Mul arg2 arg2))
p' (Mul arg1 arg2) = (Sum (Mul (myP arg1) arg2) (Mul arg1 (myP arg2)))
p' (Sub arg1 arg2) = (Sub (myP arg1) (myP arg2))
p' (Var) = Const 1
p' (Const a) = Const 0

myP = simplify.p'

getDegreeBrack Var = "x"
getDegreeBrack (Const a) = show a
getDegreeBrack a = "(" ++ show a ++ ")"

getDMBrack Var = "x"
getDMBrack (Const a) = show a
getDMBrack (Degree a b) = show (Degree a b)
getDMBrack a = "(" ++ show a ++ ")"

instance Show a => Show (FluxType a) where
	show (Degree var deg) = (getDegreeBrack var) ++ "^" ++ show deg
	show (Sum arg1 arg2) = show arg1 ++ "+" ++ show arg2
	show (Divis arg1 arg2) = getDMBrack arg1 ++ "/" ++ getDMBrack arg2
	show (Mul arg1 arg2) = getDMBrack arg1 ++ "*" ++ getDMBrack arg2
	show (Sub arg1 arg2) = show arg1 ++ "-" ++ show arg2
	show (Var) = "x"
	show (Const a) = show a

getFlux :: FluxType Int -> String
getFlux v = show (myP v)