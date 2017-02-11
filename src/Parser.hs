module Parser 
    ( parser,
      symbol,
      parseString,
      parseAtom,
      LispVal(..), 
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
             | Character Char 
             deriving (Show, Eq)

parser :: IO ()
parser = do
    (expr:_) <- getArgs
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
    c <- oneOf "\\\"0nrvtbf"
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

character :: Parser String
character = fmap return nonEscape <|> escape

parseAtom:: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first:rest

parseHash:: Parser LispVal
parseHash = do
    a <- char '#'
    b <- parseBool <|> parseChar <|> parseBase
    return b

parseBool:: Parser LispVal
parseBool = do
    v <- oneOf "tf"
    return $ Bool $ case v of
                't' -> True
                'f' -> False

parseChar:: Parser LispVal
parseChar = do
    a <- char '\\'
    b <- try (string "newline" <|> string "space") <|> parseSingleChar
    return $ Character $ case b of
                "space" -> ' '
                "newline" -> '\n'
                _ -> head b

parseBase:: Parser LispVal 
parseBase = do
    a <- oneOf "doh"
    v <- many1 digit 
    return $ Number . toInteger $ case a of 
                'd' -> extract $ readDec v
                'o' -> extract $ readOct v
                'h' -> extract $ readHex v
    where
        extract = fst . head


parseSingleChar:: Parser String
parseSingleChar = do
    a <- anyChar
    b <- char ' ' <|> char ')' <|> char '('
    return [a];

parseNumber:: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr:: Parser LispVal
parseExpr = try parseNumber 
         <|> try parseBool 
         <|> try parseChar 
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> parseParens

parseParens:: Parser LispVal
parseParens = do
    char '('
    a <- parseExpr `sepEndBy` spaces
    mayb <- optionMaybe (char '.' >> spaces >> parseExpr)
    char ')'
    return $ case mayb of 
      Nothing -> List a
      Just b -> DottedList a b

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

spaces:: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val 
