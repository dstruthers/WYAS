module Main where
import Control.Monad
import Eval
import Parser
import System
import Types

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
