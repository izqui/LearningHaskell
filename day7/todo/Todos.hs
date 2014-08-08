module Todos where

import System.IO
import System.Directory
import System.Environment
import Data.List

main = do
	(command:args) <- getArgs
	dispatch command args

dispatch :: String -> [String] -> IO ()
dispatch "add" xs = add xs
dispatch "view" xs = view xs
dispatch "remove" xs  = remove xs
dispatch _ _ = putStrLn "Incorrect command, yo"

add :: [String] -> IO ()
add [filename, name] = do
						contents <- readFile filename
						let todos = readTodos contents
						writeTodos filename (todos ++ [Todo {text = name}])
add _ = putStrLn "Incorrect syntax for add"

view :: [String] -> IO ()
view [filename] = do
					contents <- readFile filename
					let todos = readTodos contents
					printTodos todos
view _ = putStrLn "Incorrect syntax for view"

remove :: [String] -> IO ()
remove [filename, number] = do
						contents <- readFile filename
						let todos = readTodos contents
						let index = read number - 1
						let newtodos = delete (todos !! index) todos
						writeTodos filename newtodos

remove _ = putStrLn "Incorrect syntax for remove"


newtype Todo = Todo {text :: String} deriving (Show, Eq)
readTodos :: String -> [Todo]
readTodos = map Todo . lines 

printTodos = doPrint 1

doPrint :: Integer -> [Todo] -> IO ()
doPrint _ [] = return ()
doPrint i (x:xs) = do 
					putStrLn $ show i ++ ". " ++ text x
					doPrint (i+1) xs

writeTodos :: String -> [Todo] -> IO ()
writeTodos f ts = do 
					(tempName, tempHandle) <- openTempFile "." "temp"
					hPutStr tempHandle (foldl (\s t -> s ++ text t ++ "\n") "" ts)
					hClose tempHandle
					removeFile f
					renameFile tempName f
