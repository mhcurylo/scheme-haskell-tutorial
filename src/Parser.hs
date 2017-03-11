module Parser
    ( readExpr,
      readExprList,
      symbol,
      parseString,
      parseAtom,
      parseHash,
      parseNumber,
      parseExpr,
      parseParens,
      parseQuoted,
    ) where

import           Control.Monad.Except
import           Data.Char
import           LispVal
import           Numeric                       (readDec, readHex, readInt,
                                                readOct)
import           Text.ParserCombinators.Parsec hiding (spaces)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many character
    _ <- char '"'
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

parseAtom :: Parser LispVal
parseAtom = do
    x <- letter <|> symbol
    xs <- many (letter <|> digit <|> symbol)
    return $ Atom $ x:xs

parseHash :: Parser LispVal
parseHash = do
    _ <- char '#'
    parseBool <|> parseChar <|> parseBase

parseBool :: Parser LispVal
parseBool = do
    v <- oneOf "tf"
    return $ Bool $ case v of
                't' -> True
                'f' -> False

parseChar :: Parser LispVal
parseChar = do
    a <- char '\\'
    b <- try (string "newline" <|> string "space") <|> parseSingleChar
    return $ Character $ case b of
                "space"   -> ' '
                "newline" -> '\n'
                _         -> head b

parseBase :: Parser LispVal
parseBase = parseBaseD <|> parseBaseH <|> parseBaseO <|> parseBaseB

parseBaseD :: Parser LispVal
parseBaseD = char 'd' >> many1 digit >>= toLispNumber . readDec

parseBaseH :: Parser LispVal
parseBaseH = char 'h' >> many1 (digit <|> oneOf "abcdef") >>= toLispNumber . readHex

parseBaseO :: Parser LispVal
parseBaseO = char 'o' >> many1 (oneOf "01234567") >>= toLispNumber . readOct

parseBaseB :: Parser LispVal
parseBaseB = char 'b' >> many1 (oneOf "01") >>= toLispNumber . readBin

readBin :: ReadS Integer
readBin = readInt 2 (`elem` "01") digitToInt

toLispNumber :: [(Integer, String)] -> Parser LispVal
toLispNumber a = return $ Number . toInteger . fst . head $ a

parseSingleChar :: Parser String
parseSingleChar = do
    x <- anyChar
    _ <- try $ lookAhead $ oneOf " ()"
    return [x];

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr =  parseNumber
         <|> parseString
         <|> parseHash
         <|> parseAtom
         <|> parseQuoted
         <|> parseParens

parseParens  :: Parser LispVal
parseParens = do
    _ <- char '('
    x <- parseExpr `sepEndBy` spaces
    mayb <- optionMaybe (char '.' >> spaces >> parseExpr)
    _ <- char ')'
    return $ case mayb of
      Nothing -> List x
      Just y  -> DottedList x y

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

spaces :: Parser ()
spaces = skipMany1 space

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (parseExpr `endBy` spaces)
