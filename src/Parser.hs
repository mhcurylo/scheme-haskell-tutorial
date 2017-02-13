module Parser 
    ( readExpr,
      symbol,
      parseString,
      parseAtom,
      parseHash,
      parseNumber,
      parseExpr,
      parseParens,
      parseQuoted,
      LispVal(..), 
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex, readDec, readOct)

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
    x <- char '\\'
    y <- oneOf "\\\"0nrvtbf"
    return [x, y]

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

character :: Parser String
character = fmap return nonEscape <|> escape

parseAtom:: Parser LispVal
parseAtom = do
    x <- letter <|> symbol
    xs <- many (letter <|> digit <|> symbol)
    return $ Atom $ x:xs

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
    x <- anyChar
    _ <- try $ lookAhead $ oneOf " ()"
    return [x];

parseNumber:: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr:: Parser LispVal
parseExpr =  parseNumber 
         <|> parseString
         <|> parseHash
         <|> parseAtom
         <|> parseQuoted
         <|> parseParens

parseParens:: Parser LispVal
parseParens = do
    char '('
    x <- parseExpr `sepEndBy` spaces
    mayb <- optionMaybe (char '.' >> spaces >> parseExpr)
    char ')'
    return $ case mayb of 
      Nothing -> List x
      Just y -> DottedList x y

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
