module LispVal 
    (  LispVal(..), 
    ) where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char 
             deriving (Eq)

instance Show LispVal where show = showLispVal

showLispVal:: LispVal -> String
showLispVal (Character x) = x:""
showLispVal (Atom x) = x
showLispVal (Number x) = show x
showLispVal (String x) = "\"" ++ x ++ "\""
showLispVal (Bool True) = "#t"
showLispVal (Bool False) = "#f"
showLispVal (List xs) = "(" ++ unwords (map showLispVal xs) ++ ")"
showLispVal (DottedList x xs) = "(" ++ unwords (map showLispVal x) ++ " . " ++ showLispVal xs ++ ")"

