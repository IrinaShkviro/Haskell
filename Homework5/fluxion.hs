data FluxType a = Degree (FluxType a) Int
	| Sum (FluxType a) (FluxType a)
	| Divis (FluxType a) (FluxType a)
	| Mul (FluxType a) (FluxType a)
	| Sub (FluxType a) (FluxType a)
	| Const a
	| Var

simple (Degree var 1) = var
simple (Degree var 0) = Const 1
simple (Sum (Const 0) arg2) = arg2
simple (Sum arg1 (Const 0)) = arg1
simple (Divis arg1 (Const 1)) = arg1
simple (Divis (Const 0) _) = Const 0
simple (Mul (Const 1) arg2) = arg2
simple (Mul arg1 (Const 1)) = arg1
simple (Mul (Const 0) _) = Const 0
simple (Mul _ (Const 0)) = Const 0
simple (Sub arg1 (Const 0)) = arg1
simple (Degree arg1 arg2) = Degree arg1 arg2
simple (Sum arg1 arg2) = Sum (simple arg1) (simple arg2)
simple (Divis arg1 arg2) = Divis (simple arg1) (simple arg2)
simple (Mul arg1 arg2) = Mul (simple arg1) (simple arg2)
simple (Sub arg1 arg2) = Sub (simple arg1) (simple arg2)
simple (Const a) = Const a
simple Var = Var

p' (Degree var deg) = (Mul (Mul (Const deg) (Degree var (deg - 1))) (myP var))
p' (Sum ad1 ad2) = (Sum (myP ad1) (myP ad2))
p' (Divis arg1 arg2) = (Divis (Sub (Mul (myP arg1) arg2) (Mul (myP arg2) arg1)) (Mul arg2 arg2))
p' (Mul arg1 arg2) = (Sum (Mul (myP arg1) arg2) (Mul arg1 (myP arg2)))
p' (Sub arg1 arg2) = (Sub (myP arg1) (myP arg2))
p' (Var) = Const 1
p' (Const a) = Const 0

myP = simple.p'

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