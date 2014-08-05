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

	fromInteger = Lit

eval' :: ExprT -> Integer
eval' (Lit i) = i
eval' (Add a b) = eval $ a + b
eval' (Mul a b) = eval $ a * b

evalStr :: String -> Maybe Integer
evalStr str 
	| res == Nothing = Nothing
	| otherwise = Just $ eval' $ unwrap res
	where res = parseExp lit add mul str

unwrap :: Maybe a -> a
unwrap Nothing = error "Unwraping nothing"
unwrap (Just x) = x

class Expr a where
	lit :: Integer -> a
	mul :: a -> a -> a
	add :: a -> a -> a

instance Expr ExprT where
	lit = Lit
	add = Add 
	mul = Mul

instance Expr Integer where
	lit = id
	add = (+)
	mul = (*)

instance Expr Bool where
	lit i = i > 0
	add = (||)
	mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Ord MinMax where
	(<=) (MinMax i) (MinMax j) = i<=j

instance Expr MinMax where
	lit = MinMax
	add = max 
	mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Num Mod7 where
	(+) (Mod7 i) (Mod7 j) = fromInteger $ i+j
	(*) (Mod7 i) (Mod7 j) = fromInteger $ i*j

	abs (Mod7 i) = fromInteger $ abs i
	signum (Mod7 i) = fromInteger $ signum i

	fromInteger i 
		| i < 7 = Mod7 i
		| otherwise = Mod7 $ mod i 7  

instance Expr Mod7 where
	lit = Mod7
	add = (+)
	mul = (*)

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul
