data BinaryTree = EmptyBinaryTree
	| Leaf Integer
	| Node Integer BinaryTree BinaryTree 
	deriving (Eq)
instance Show BinaryTree where 
	show EmptyBinaryTree = "<>"	
	show (Leaf c) = show c
	show (Node c l r) = "<" ++ show l ++ "|" ++ show c ++ "|" ++ show r ++ ">"	
	
emptyTree = EmptyBinaryTree

insert (EmptyBinaryTree) x = Leaf x
insert (Leaf c) x 
	| c < x 	= Node c EmptyBinaryTree (Leaf x)
	| c == x	= Leaf c
	| c > x 	= Node c (Leaf x) EmptyBinaryTree
insert (Node c l r) x 
	| c < x 	= Node c l (insert r x)
	| c == x	= Node c l r
	| c > x		= Node c (insert l x) r

listFromTree EmptyBinaryTree = []
listFromTree (Leaf c) = [c]
listFromTree (Node c l r) = (:) c $ (++) (listFromTree l) $ listFromTree r

remove EmptyBinaryTree x = EmptyBinaryTree
remove (Leaf c) x 
	| c == x	= EmptyBinaryTree
	| c /= x	= Leaf c
remove (Node c EmptyBinaryTree r) x 
	| c < x		= if ur == EmptyBinaryTree then Leaf c else Node c EmptyBinaryTree ur 
	| c == x	= r
	| c > x		= Node c EmptyBinaryTree ur
		where ur 	= remove r x
remove (Node c l EmptyBinaryTree) x
	| c < x 	= Node c ul EmptyBinaryTree
	| c == x 	= l
	| c > x		= if ul == EmptyBinaryTree then Leaf c else Node c ul EmptyBinaryTree 
		where ul = remove l x
remove (Node c l r) x
	| c < x 	= Node c l $ remove r $ x
	| c == x	= foldl insert r $ listFromTree l
	| c > x 	= Node c (remove l x) r 
	
treeFromList xs = foldl insert EmptyBinaryTree xs

containsElement EmptyBinaryTree x = False
containsElement (Leaf c) x = c == x
containsElement (Node c l r)  x
	| c < x 	= containsElement r x
	| c == x 	= True
	| c > x		= containsElement l x

nearestGE EmptyBinaryTree x = Nothing
nearestGE (Leaf c) x 
	| c >= x	= Just c
	| c < x		= Nothing
nearestGE (Node c l r) x
	| c >= x 	= 
		let y = nearestGE l x
		in
			if y == Nothing then Just c else y 
	| c < x		= nearestGE r x