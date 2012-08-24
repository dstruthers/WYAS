module Types where
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Char Char
             | Bool Bool

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ showListContents contents ++ ")"
showVal (DottedList head tail) = "(" ++ showListContents head ++ " . " ++ showVal tail ++ ")"
showVal (Number n) = show n
showVal (Float d) = show d
showVal (String s) = "\"" ++ s ++ "\""
showVal (Char c) = "\\#" ++ case c of
  ' '  -> "space"
  '\n' -> "newline"
  '\t' -> "\\t"
  '\r' -> "\\r"
  _    -> [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

showListContents = unwords . map showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form 
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                     ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr                                          

instance Show LispError where
  show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
