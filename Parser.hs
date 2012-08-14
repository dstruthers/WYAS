module Parser where
import Control.Monad
import Data.List (intercalate)
import Numeric (readOct, readHex, readFloat)
import Text.ParserCombinators.Parsec hiding (spaces)
import Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
                 
parseChar :: Parser LispVal
parseChar = do string "#\\"
               c <- (string "space" <|> string "newline" <|> singleChar)
               return $ Char $ case c of
                 "space"   -> ' '
                 "newline" -> '\n'
                 _         -> head c
  where singleChar = do x <- anyChar
                        notFollowedBy alphaNum
                        return [x]

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do char '#'
               b <- oneOf "tf"
               return $ case b of
                 't' -> Bool True
                 'f' -> Bool False

parseFloat :: Parser LispVal
parseFloat = do n <- many1 digit
                char '.'
                dec <- many1 digit
                return $ Float $ fromFloat $ n ++ "." ++ dec
  where fromFloat = fst . head . readFloat

parseNumber :: Parser LispVal
parseNumber = do base <- try (char '#' >> oneOf "ox") <|> return 'd'
                 case base of 
                   'b' -> many1 (oneOf "01") >>= return . Number . fromBin
                   'd' -> many1 digit >>= return . Number . read
                   'o' -> many1 octDigit >>= return . Number . fromOct
                   'x' -> many1 hexDigit >>= return . Number . fromHex
  where fromOct = fst . head . readOct        
        fromHex = fst . head . readHex
        fromBin = fromBin' 0
        fromBin' acc ""     = acc
        fromBin' acc (c:cs) = let c' = if c == '1' then 1 else 0
                                  acc' = 2 * acc + c'
                              in fromBin' acc' cs


parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepEndBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- parseExpr `endBy` spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail  
  
parseQuote :: Parser LispVal
parseQuote = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]
  
parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseChar
        <|> parseQuote
        <|> parseQuasiQuote
        <|> parseUnquote
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "No match: " ++ show err
  Right val -> val
