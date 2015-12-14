main = interact showFactorial

factorial n = factorial' n 1

factorial' 1 p  = p
factorial' n p = let m = n - 1 in factorial' m $ n * p

showFactorial x = unlines $ map readEvaluateAndShow $ lines x
	where readEvaluateAndShow = show.factorial.read