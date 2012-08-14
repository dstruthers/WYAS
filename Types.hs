module Types where
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