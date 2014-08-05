module Calc where

import ExprT

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

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval $ Lit ((eval a) + (eval b))
eval (Mul a b) = eval $ Lit ((eval a) * (eval b))

eval' :: ExprT -> Integer
eval' (Lit i) = i
eval' (Add a b) = eval $ a + b
eval' (Mul a b) = eval $ a * b