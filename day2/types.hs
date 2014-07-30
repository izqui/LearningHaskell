data Point = Point Float Float deriving (Show)  
data Size = Size Float Float deriving (Show)  

data Shape = Circle Point Float | Rectangle Point Size deriving (Show)  

area :: Shape -> Float
area (Circle _ r) = pi * (r*r)
area (Rectangle _ (Size w h)) = w * h

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

data Person = Person {
	
	name :: String, 
	age :: Maybe Int
} deriving (Show)