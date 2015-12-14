-- for some tests
-- let a = Snoc (Snoc (Snoc RNil 10) 1) 3
-- let b = Snoc (Snoc (Snoc RNil 9) 1) 4
import Data.Monoid

data RevList a = Snoc (RevList a) a | RNil

toString :: (Show a) => (RevList a) -> String
toString RNil 			= ""
toString (Snoc RNil l) 	= show l 
toString (Snoc i l) 	= (show l) ++ ", " ++ (toString i)

instance (Show a) => Show (RevList a) where 
	show rxs = "[" ++ (toString rxs) ++ "]"  	
	
instance (Eq a) => Eq (RevList a) where
	RNil == RNil 					= True
	_ == RNil						= False
	RNil == _						= False
	(Snoc li ll) == (Snoc ri rl) 	= (ll == rl) && (li == ri)
	
instance (Ord a) => Ord (RevList a) where
	RNil <= RNil 					= True 
	_ <= RNil						= False
	RNil <= _						= True
	(Snoc li ll) <= (Snoc ri rl) 
		| (ll <= rl) 				= True
		| otherwise					= ll <= rl	
		
instance Monoid (RevList a) where 
	mempty = RNil
	mappend RNil RNil 		= RNil
	mappend rxs RNil 		= rxs
	mappend RNil rys		= rys
	mappend (Snoc i l) rxs	= Snoc (mappend i rxs) l
	
instance Functor RevList where 
	fmap f RNil 		= RNil
	fmap f (Snoc i l) 	= Snoc (fmap f i) $ f l
	
instance Monad RevList where
	return x 			= Snoc RNil x
	RNil >>= f 			= RNil
	(Snoc i l) >>= f 	= mappend (f l) $ i >>= f