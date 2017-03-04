module LispVal 
    (  LispVal(..), 
       LispError(..),
       ThrowsError,
       trapError,
       extractValue
    ) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

showError:: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ (unwords . map show) found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default err)             = "Default error at " ++ show err

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
