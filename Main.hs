module Main where
import Parser
import System

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (head args))
