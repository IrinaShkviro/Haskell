import System.Environment
import Control.Monad

data Graph v e = Graph [(Int, v)] [(Int,Int,e)]
data MyGraph v e = MyGraph [(Int, v, e, Bool)] [(Int,Int, e)]

f1 :: (Eq v, Fractional e) => (Int, v) -> (Int, v) -> (Int, v, e, Bool)
f1 (id, v) src = if ((id, v) == src) 
	then (id, v, 0, False)
	else (id, v, 1/0, False)

foreach' :: ((Int, v) -> (Int, v) -> (Int, v, e, Bool)) -> [(Int, v)] -> (Int, v) -> [(Int, v, e, Bool)]
foreach' _ [] _ = []
foreach' f (x : xs) src = ((f x src) : (foreach' f xs src))

initialize :: (Eq v, Fractional e) => Graph v e -> (Int, v) -> MyGraph v e
initialize (Graph v el) src = MyGraph (foreach' f1 v src) el

findMin :: (Ord e) => MyGraph v e -> (Int, v, e, Bool) -> (Int, v, e, Bool)
findMin (MyGraph [] _ ) min = min
findMin (MyGraph (v : vl) el) min = let 
	(cura, curi, curb, curc) = v
	(mina, mini, minb, minc) = min
	newmin = if (not curc && curb <= minb)
		then v
		else min 
	in (findMin (MyGraph vl el) newmin)

vis :: (Ord e, Num e) => (Int, v, e, Bool) -> (Int, v, e, Bool) -> [(Int, Int , e)] -> (Int, v, e, Bool)
vis _ cFinish [] = cFinish
vis (cSid, cSval, cSdist, cSvis) (cFid, cFval, cFdist, cFvis) ((start, end, len) : ls) = 
	if (cFvis)
	then (cFid, cFval, cFdist, cFvis)
	else
		if (cSid == start && cFid == end)
		then
			if (cFdist > cSdist + len)
			then (cFid, cFval, cSdist + len, cFvis)
			else (cFid, cFval, cFdist, cFvis)
		else vis (cSid, cSval, cSdist, cSvis) (cFid, cFval, cFdist, cFvis) ls

visit :: (Ord e, Num e) => (Int, v, e, Bool) -> MyGraph v e -> [(Int, v, e, Bool)]
visit cur (MyGraph [] el) = []
visit cur (MyGraph (v : vs) el) = ((vis cur v el) : visit cur (MyGraph vs el))

mark :: (Int, v, e, Bool) -> [(Int, v, e, Bool)] -> [(Int, v, e, Bool)]
mark (verId, verVal, verDist, verVis) ((vId, vVal, vDist, vVis) : vs) = 
	if (verId == vId)
	then ((verId, verVal, verDist, not verVis) : vs)
	else ((vId, vVal, vDist, vVis) : (mark (verId, verVal, verDist, verVis) vs))

helper :: (Ord e, Num e, Fractional e, Num v) => MyGraph v e -> MyGraph v e
helper (MyGraph vl el) = let
	(minId, minVal, minDist, minVis) = findMin (MyGraph vl el) (0, 0, 1/0, True)
	min = (minId, minVal, minDist, minVis)
	result = if (minVis) 
		 then (MyGraph vl el)
		 else  helper $ MyGraph (visit min (MyGraph (mark min vl) el)) el
	in result

algo :: (Eq v, Num v, Ord e, Fractional e) => Graph v e -> (Int, v) -> MyGraph v e
algo g start = let
	newG = initialize g start 
	in helper newG

showGraph :: (Show v, Show e) => MyGraph v e -> String
showGraph (MyGraph v e) = show v

test = Graph [(1, 3), (2, 4), (3, 1), (4, 5), (5, 1), (6, 3)] [(1, 2, 4), (1, 3, 3), (1, 4, 7), (2, 4, 4), (2, 5, 2), (3, 5, 3), (4, 6, 3), (5, 6, 2)]
main = print $ showGraph $ algo test (1, 3) 