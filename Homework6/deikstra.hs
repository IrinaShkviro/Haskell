import System.Environment
import Control.Monad

data Graph a e = Graph [(Int, a)] [(Int,Int,e)]
data MyGraph v a e = MyGraph [(Int, a, v, Bool)] [(Int,Int, e)]

f1 (v, id) src = if ((v, id) == src) 
	then (v, id, 0, False)
	else (v, id, 1/0, False)

foreach' _ [] _ = []
foreach' f (x : xs) src = ((f x src) : (foreach' f xs src))

initialize (Graph v el) src = MyGraph (foreach' f1 v src) el

findMin (MyGraph [] _ ) min = min
findMin (MyGraph (v : vl) el) min = let 
	(cura, curi, curb, curc) = v
	(mina, mini, minb, minc) = min
	newmin = if (not curc && curb <= minb)
		then v
		else min 
	in (findMin (MyGraph vl el) newmin)

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

visit cur (MyGraph [] el) = []
visit cur (MyGraph (v : vs) el) = ((vis cur v el) : visit cur (MyGraph vs el))

mark (verId, verVal, verDist, verVis) ((vId, vVal, vDist, vVis) : vs) = 
	if (verId == vId)
	then ((verId, verVal, verDist, not verVis) : vs)
	else ((vId, vVal, vDist, vVis) : (mark (verId, verVal, verDist, verVis) vs))

helper (MyGraph vl el) = let
	(minId, minVal, minDist, minVis) = findMin (MyGraph vl el) (0, 0, 1/0, True)
	min = (minId, minVal, minDist, minVis)
	result = if (minVis) 
		 then (MyGraph vl el)
		 else  helper $ MyGraph (visit min (MyGraph (mark min vl) el)) el
	in result

algo g start = let
	newG = initialize g start 
	in helper newG

showGraph (MyGraph v e) = show v