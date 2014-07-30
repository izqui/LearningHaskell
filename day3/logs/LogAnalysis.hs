module LogAnalysis where

import Log
import Data.List

timestamp :: LogMessage -> Int
timestamp (LogMessage _ t _) = t
timestamp _ = 0

value :: MessageTree -> LogMessage
value (Node _ l _) = l
value _ = Unknown "unknown"

left :: MessageTree -> MessageTree
left (Node l _ _) = l
left _ = Leaf

right :: MessageTree -> MessageTree
right (Node _ _ r) = r
right _ = Leaf

treeInsert :: MessageTree -> LogMessage -> MessageTree
treeInsert Leaf x = Node Leaf x Leaf
treeInsert t x
	| xi <= self = Node (treeInsert (left t) x) (value t) (right t)
	| xi > self =  Node (left t) (value t) (treeInsert (right t) x)
	where self = (timestamp . value) t 
	      xi = timestamp x

build :: [LogMessage] -> MessageTree
build [] = Leaf
build xs = foldl treeInsert Leaf xs

order :: MessageTree -> [LogMessage]
order Leaf = []
order t = (order . left) t ++ [value t] ++ (order . right) t

parse :: String -> [LogMessage]
parse = map parseMessage . lines --split '\n'

parseMessage :: String -> LogMessage
parseMessage x
	| identifier == "I" = LogMessage Info (read $ parsed !! 1) (unwords $ drop 2 parsed)
	| identifier == "W" = LogMessage Warning (read $ parsed !! 1) (unwords $ drop 2 parsed)
	| identifier == "E" = LogMessage (Error (read $ parsed !! 1)) (read $ parsed !! 2) (unwords $ drop 3 parsed)
	| otherwise = Unknown x 
	where parsed = split ' ' x
	      identifier = head parsed

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split s xs = foldl'' sp [[]] s xs

sp :: (Eq a) => [[a]] -> a -> a -> [[a]]
sp ac bc x 
	| x == bc = ac ++ [[]]
	| otherwise = init ac ++ [last ac ++ [x]]

foldl'' :: (a -> b -> b -> a) -> a -> b -> [b] -> a
foldl'' _ ac _ [] = ac
foldl'' f ac bc (x:xs) = foldl'' f (f ac bc x) bc xs