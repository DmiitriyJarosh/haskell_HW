module Main where

import Prelude
import Control.Monad
import Eval
import Parser

main :: IO ()
main = do
	putStrLn ("Test1: " ++ show Parser.test1)
	case mainEval $ myParse Parser.test1 of
		Left str -> putStrLn ("Result: " ++ str)
		Right terms -> putStrLn ("Result: " ++ show terms)
	putStrLn ("Test2: " ++ show Parser.test2)
	case mainEval $ myParse Parser.test2 of
		Left str -> putStrLn ("Result: " ++ str)
		Right terms -> putStrLn ("Result: " ++ show terms)
	putStrLn ("Test3: " ++ show Parser.test3)
	case mainEval $ myParse Parser.test3 of
		Left str -> putStrLn ("Result: " ++ str)
		Right terms -> putStrLn ("Result: " ++ show terms)
	putStrLn ("Test4: " ++ show Parser.test4)
	case mainEval $ myParse Parser.test4 of
		Left str -> putStrLn ("Result: " ++ str)
		Right terms -> putStrLn ("Result: " ++ show terms)
	putStrLn ("Test5: " ++ show Parser.test5)
	case mainEval $ myParse Parser.test5 of
		Left str -> putStrLn ("Result: " ++ str)
		Right terms -> putStrLn ("Result: " ++ show terms)
	putStrLn ("Test6: " ++ show Parser.test6)
	case mainEval $ myParse Parser.test6 of
		Left str -> putStrLn ("Result: " ++ str)
		Right terms -> putStrLn ("Result: " ++ show terms)
  

