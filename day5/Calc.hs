module Calc where

import ExprT
import Parser

-- Naive way
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval $ Lit ((eval a) + (eval b))
eval (Mul a b) = eval $ Lit ((eval a) * (eval b))

-- Super cool way
instance Num ExprT where
	(+) (Lit i) (Lit j) = fromInteger $ i+j
	(+) i j = Add i j

	(*) (Lit i) (Lit j) = fromInteger $ i*j
	(*) i j = Mul i j

	abs (Lit i) = fromInteger $ abs i
	abs _ = fromInteger 0

	signum (Lit i) = fromInteger $ signum i
	signum _ = fromInteger 1

	fromInteger i = Lit i

eval' :: ExprT -> Integer
eval' (Lit i) = i
eval' (Add a b) = eval $ a + b
eval' (Mul a b) = eval $ a * b

evalStr :: String -> Maybe Integer
evalStr str 
	| res == Nothing = Nothing
	| otherwise = Just $ eval' $ unwrap res
	where res = parseExp Lit Add Mul str

unwrap :: Maybe a -> a
unwrap Nothing = error "Unwraping nothing"
unwrap (Just x) = x