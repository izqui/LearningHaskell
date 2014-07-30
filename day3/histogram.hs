module Histogram where

import Data.List

empty = replicate 10 0
iDraw = "\n==========\n0123456789\n"

histogram :: [Int] -> String
histogram = draw . parse

parse :: [Int] -> [Int]
parse [] = empty
parse xs = foldl act empty xs 

act :: [Int] -> Int -> [Int]
act ac x = take x ac ++ [(ac !! x)+1] ++ drop (x+1) ac

draw :: [Int] -> String
draw [] = iDraw
draw xs = intercalate "\n" (reverse $ dlines xs) ++ iDraw 

dlines :: [Int] -> [String]
dlines xs 
	| xs == replicate 10 0 = []
	| otherwise = [if x > 0 then '*' else ' ' | x <- xs] : (dlines . map substract) xs 
	where substract = \ x -> if x == 0 then x else (x - 1)
