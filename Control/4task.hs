f :: [Double] -> Double -> Double -> Double
f [] sum pcos = sum / pcos
f (l : ls) sum pcos = f ls (sum + l) (pcos * (cos l))

res list = f list 0 1