import Prelude hiding (foldr, foldl, map, flatMap, filter, reverse, last)

foldl _ x [] 		= x
foldl f x (y:ys) 	= foldl f (f x y) ys

foldr _ x [] 		= x
foldr f x (y:ys) 	= f y $ foldr f x ys

mapFirstArg mapping f = \x y -> let mx = mapping x in f mx y

map f xs = foldr (mapFirstArg f (:)) [] xs

flatMap f xs = foldr f' [] xs where f' = mapFirstArg f (++)

filter f xs = foldr f' [] xs where f' = mapFirstArg (\x -> if f x then [x] else []) (++)

append xs [] = xs
append [] ys = ys
append xs ys = foldr (:) ys xs

reverse xs = foldl (flip (:)) [] xs 

last [] = error "Not exists"
last xs = foldl const 0 xs

snoc xs x = foldr (:) [x] xs