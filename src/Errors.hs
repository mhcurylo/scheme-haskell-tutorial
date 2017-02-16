module Errors (
      LispError(..),
      trapError,
      extractValue
    ) where 

import Control.Monad.Except
import LispVal
import Text.ParserCombinators.Parsec hiding (spaces)

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

instance MonadError LispErr where


trapError action = catchError action (return . show)

extractValue :: LispErr a -> a
extractValue (Right val) = val
