module Parser 
    ( parser,
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
             deriving (Show)

parser :: IO ()
parser = do
    (expr:_) <- getArgs
    putStrLn (readExpr "\"this is \\\"test \\\" message \\\"sample\\\" text\"")
    putStrLn (readExpr "22")
    putStrLn (readExpr "#o22")
    putStrLn (readExpr "#\\A")
    putStrLn (readExpr "space")
    putStrLn (readExpr "#t")
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
    return $ Atom $ first:rest

parseBool:: Parser LispVal
parseBool = do
    a <- char '#'
    b <- oneOf "tf"
    let v = [a, b]
    return $ Bool $ case v of
                "#t" -> True
                "#f" -> False

parseChar:: Parser LispVal
parseChar = do
    a <- string "#\\"
    b <- try (string "newline" <|> string "space") <|> parseSingleChar
    return $ Character $ case b of
                "space" -> ' '
                "newline" -> '\n'
                _ -> head b

parseSingleChar:: Parser String
parseSingleChar = do
    a <- anyChar
    b <- char ' ' <|> char ')' <|> char '('
    return [a];

parseNumber:: Parser LispVal
parseNumber = Number <$> (intBase10 <|> try intBase)

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
parserExpr = try parseNumber <|> try parseBool <|> try parseChar <|> parseAtom <|> parseString

spaces:: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parserExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val 
