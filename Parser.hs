module Parser where
import Control.Monad
import Data.List (intercalate)
import Numeric (readOct, readHex)
import Text.ParserCombinators.Parsec hiding (spaces)
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChar :: Parser Char
escapedChar = do char '\\'
                 x <- oneOf "\\\"nrt"
                 return (translate x)
  where translate c = case c of
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          _   -> c

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChar <|> noneOf "\\\"")
                 char '"'
                 return $ String x
                 
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do base <- try (char '#' >> oneOf "ox") <|> return 'd'
                 case base of 
                   'd' -> many1 digit >>= return . Number . read
                   'o' -> many1 (oneOf "01234567") 
                            >>= return . Number . fst . head . readOct
                   'x' -> many1 (digit <|> oneOf "abcdefABCDEF")
                            >>= return . Number . fst . head . readHex

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value"
