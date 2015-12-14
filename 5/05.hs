module Calc (calculate) where

import Text.Parsec
import Control.Applicative hiding ((<|>), many)

type Parser a = Parsec String () a

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

positive 		= many1 digit
negative 		= char '-' <:> positive
signedNumber 	= positive <|> negative
doubleNumber 	= fmap rd $ positive <++> decimal <++> exponent
	where 	
		rd       = read :: String -> Double
		decimal  = option "" $ char '.' <:> positive
		exponent = option "" $ oneOf "eE" <:> signedNumber

plus 			= const (+) <$> char '+'
minus 			= const (-) <$> char '-'
multiplication 	= const (*) <$> char '*'
division 		= const (/) <$> char '/'

sinus 		= const sin <$> string "sin" 
cosine 		= const cos <$> string "cos"  
logarithm 	= const log <$> string "log" 
squareRoot 	= const sqrt <$> string "sqrt"  

preceededBySpaces p = p <* spaces
followedBySpaces p  = spaces *> p

anyFunctionName = followedBySpaces $ try sinus <|> cosine <|> logarithm <|> squareRoot
anyFunction 	= anyFunctionName <*> expresionInParentheses

signedExpression = followedBySpaces sign <*> unsignedExpression
	where 
		sign 				= fmap parseSign $ option '+' $ char '-'
		parseSign '+' 		= (0+)
		parseSign '-' 		= (0-)
		unsignedExpression 	= try expresionInParentheses <|> doubleNumber <|> anyFunction
		
evaluate :: a -> [a -> a] -> a
evaluate initial ops = foldl (\x f -> f x) initial ops		
		
opWithArgument op argument	= flip <$> op <*> argument
		
productExpression = evaluate <$> preceededBySpaces signedExpression <*> others
	where 
		others 			= many $ opWithArgument op $ preceededBySpaces signedExpression
		op 				= preceededBySpaces $ multiplication <|> division

sumExpression = evaluate <$> productExpression <*> others
	where 
		others 			= many $ opWithArgument op $ followedBySpaces productExpression
		op 				= followedBySpaces $ plus <|> minus
		
expresionInParentheses = leftParenthesis *> (<*) sumExpression rightParenthesis
	where
		leftParenthesis 	= preceededBySpaces $ char '('
		rightParenthesis 	= followedBySpaces $ char ')'
		
fullExpression = sumExpression
calculate :: String -> Either ParseError Double
calculate text = runParser fullExpression () "user-supplied" text