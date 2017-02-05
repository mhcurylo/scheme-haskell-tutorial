module Lib
    ( someFunc,
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readHex, readDec, readOct)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

someFunc :: IO ()
someFunc = do
    (expr:_) <- getArgs
    putStrLn (readExpr "\"this is \\\"test \\\" message \\\"sample\\\" text\"")
    putStrLn (readExpr "22")
    putStrLn (readExpr "#o22")
    putStrLn (readExpr expr)

symbol:: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString:: Parser LispVal
parseString = do
    char '"'
    x <- many character
    char '"'
    return $ String $ concat x

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

character :: Parser String
character = fmap return nonEscape <|> escape

parseAtom:: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseNumber:: Parser LispVal
parseNumber = Number <$> (intBase10 <|> intBase)

intBase10:: Parser Integer
intBase10 = read <$> many1 digit

intBase:: Parser Integer 
intBase = do
    a <- char '#'
    b <- oneOf "doh"
    v <- many1 digit 
    let base = [a, b]
    return $ toInteger $ case base of 
                "#d" -> extract $ readDec v
                "#o" -> extract $ readOct v
                "#h" -> extract $ readHex v
    where
        extract = fst . head


parserExpr:: Parser LispVal
parserExpr = parseNumber <|> parseAtom <|> parseString

spaces:: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parserExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val 
