-- for some test
-- let d = pushBack (pushBack (pushBack (pushBack (pushBack (pushBack emptyDeque 1) 2) 3) 4) 5) 6

data Deque a = Deque Int [a] Int [a] deriving (Show)

reverseTail :: [a] -> Int -> [a] 
reverseTail xs n = reverse $ drop n xs

makeDeque :: Int -> [a] -> Int -> [a] -> Deque a
makeDeque lx xs ly ys
    | ly > 3 * lx + 1   = 
		let n = div (lx + ly) 2 
			in let 
				lx' = (lx + ly - n)
				xs' = xs ++ (reverseTail ys n)
				ys' = take n ys
				in Deque lx' xs' n ys'
    | lx > 3 * ly + 1   = 
		let n = div (lx + ly) 2 
			in let 
				xs' = take n xs
				ly' = (lx + ly - n)
				ys' = ys ++ (reverseTail xs n)
				in Deque n xs' ly' ys'
	| otherwise  		= Deque lx xs ly ys

emptyDeque :: Deque a
emptyDeque = Deque 0 [] 0 []

popFront :: Deque a -> (Maybe a, Deque a)
popFront d@(Deque 0 [] 0 [])        = (Nothing, d)
popFront (Deque 0 [] 1 [y])         = (Just y, Deque 0 [] 0 [])
popFront d@(Deque lx (hx:tx) ly ys) = (Just hx, makeDeque (lx - 1) tx ly ys)

popBack :: Deque a -> (Maybe a, Deque a)
popBack d@(Deque 0 [] 0 [])        = (Nothing, d)
popBack (Deque 1 [x] 0 [])         = (Just x, Deque 0 [] 0 [])
popBack d@(Deque lx xs ly (hy:ty)) = (Just hy, makeDeque lx xs (ly - 1) ty)

pushFront :: Deque a -> a -> Deque a
pushFront (Deque lx xs ly ys) x = makeDeque (lx + 1) (x:xs) ly ys

pushBack :: Deque a -> a -> Deque a
pushBack (Deque lx xs ly ys) y = makeDeque lx xs (ly + 1) (y:ys)