module ParserSpec 
  (  parserSpec,
  ) where

import Test.QuickCheck
import Data.List (intersperse)
import Text.ParserCombinators.Parsec
import Control.Monad
import Parser
import Numeric (readHex, readDec, readOct)



symbols = "!#$%&|*+-/:<=>?@^_~"
alphas = ['a'..'z'] ++ ['A'..'Z']
numerics = ['0'..'9']
escapes = "\"\\0nrvtbf"

stringOf = listOf1 . elements 

surround:: String -> String -> String -> String
surround x y = (x ++) . (++ y)


arbitrarySymbol = stringOf symbols
arbitraryNumeric = stringOf numerics
arbitraryAlpha = stringOf alphas
arbitraryAlphaNumeric = stringOf (numerics ++ alphas) 
arbitraryAlphaSymbolic = stringOf (symbols ++ alphas) 
arbitraryAlphaSymbolicNumeric = stringOf (symbols ++ numerics ++ alphas) 
arbitraryString = surround "\"" "\"" <$> arbitraryAlphaSymbolicNumeric
arbitraryAtom = suchThat atom' (\(x:y:s) -> notElem x "#" )
                  where atom' = liftM2 (++) arbitraryAlphaSymbolic arbitraryAlphaSymbolicNumeric
arbitraryEscape = (\x -> ['\\', x]) <$> elements escapes
arbitraryStringWithEscape =surround "\"" "\""<$> liftM2 (++) arbitraryAlphaSymbolicNumeric arbitraryEscape
arbitraryBool = (\x -> ['#', x]) <$> elements "tf"
arbitraryChar = oneof [elements ["#\\space", "#\\newline"], arbitrarySingleChar] 
arbitrarySingleChar = liftM2 (\x y -> "#\\" ++ [x, y]) (elements (symbols ++ alphas ++ numerics)) (elements " ()")
arbitraryCharSafe = oneof [elements ["#\\space", "#\\newline"], surround "#\\" "" . (:[])<$> elements (symbols ++ alphas ++ numerics)]
arbitraryBaseNum = (\x -> "#d" ++ x) <$> arbitraryNumeric
arbitraryQuoted = (\x -> "\'" ++ x ++ " ") <$> resize 1 arbitraryLispValString
arbitraryParens = surround "(" ")" . init <$> arbitraryManySExp
arbitraryManySExp = concat <$> listOf1 arbitrarySpacedLispVal
arbitrarySpacedLispVal = surround "" " " <$> arbitraryLispValString
arbitraryLispValString = frequency [(10, arbitraryCharSafe),
                                    (2, arbitraryBool), 
                                    (10, arbitraryBaseNum), 
                                    (10, arbitraryString),
                                    (10, arbitraryAtom),
                                    (10, arbitraryQuoted),
                                    (10, arbitraryNumeric),
                                    (1, arbitraryParens)]
arbitraryExpr          = frequency [(10, arbitraryChar),
                                    (2, arbitraryBool), 
                                    (10, arbitraryBaseNum), 
                                    (10, arbitraryString),
                                    (10, arbitraryAtom),
                                    (1, arbitraryParens),
                                    (10, arbitraryQuoted),
                                    (10, arbitraryNumeric)]

parserConfirm:: Parser a -> Bool -> String -> Bool
parserConfirm p b x = b == case parse p "" x of
                     Left err  -> False
                     Right val -> True
                     
parserLog:: Parser a -> Bool -> String -> Property
parserLog p i x = collect x $ parserConfirm p i x

parserEq:: Eq a => Parser a -> (String -> a) -> String -> Bool
parserEq p g x = case parse p "" x of 
                     Left err -> False
                     Right val -> val == g x 

schemeBool:: String -> LispVal
schemeBool "#t" = Bool True
schemeBool "#f" = Bool False

schemeChar:: String -> LispVal
schemeChar "#\\space" = Character ' '
schemeChar "#\\newline" = Character '\n'
schemeChar (x:xs) = Character (xs !! 1) 

schemeString:: String -> LispVal
schemeString = String . tail . init

schemeBaseNum:: String -> LispVal
schemeBaseNum (x:y:ys) 
    | y == 'd' = extractNum $ readDec ys
    | y == 'o' = extractNum $ readOct ys
    | y == 'h' = extractNum $ readHex ys
    where 
        extractNum = Number .fst . head

schemeNum:: String -> LispVal
schemeNum = Number . fst . head . readDec

prop_SymbolRight = forAll arbitrarySymbol $ parserEq symbol head
prop_SymbolLeft = forAll arbitraryAlphaNumeric $ parserConfirm symbol False

prop_StringRight = forAll arbitraryString $ parserEq parseString schemeString
prop_StringLeft = forAll arbitraryAlphaSymbolicNumeric $ parserConfirm parseString False
prop_StringEscape = forAll arbitraryStringWithEscape $ parserEq parseString schemeString

prop_AtomRight = forAll arbitraryAtom $ parserEq parseAtom Atom
prop_AtomLeft = forAll arbitraryNumeric $ parserConfirm parseAtom False

prop_BooleanRight = forAll arbitraryBool $ parserEq parseHash schemeBool
prop_HashLeftAN = forAll arbitraryAlphaNumeric $ parserConfirm parseHash False 
prop_HashLeftAtom = forAll arbitraryAtom $ parserConfirm parseHash False 

prop_CharRight = forAll arbitraryChar $ parserEq parseHash schemeChar

prop_BaseNumRight = forAll arbitraryBaseNum $ parserEq parseHash schemeBaseNum

prop_QuotedRight = forAll arbitraryQuoted $ parserConfirm parseQuoted True

prop_NumRight = forAll arbitraryNumeric $ parserEq parseNumber schemeNum
prop_NumLeft = forAll arbitraryAlphaSymbolic $ parserConfirm parseNumber False 

prop_ParensRight = forAll arbitraryParens $ parserConfirm parseParens True
prop_ParensLeft = forAll arbitraryAlphaSymbolicNumeric $ parserConfirm parseParens False

prop_ExprRight = forAll arbitraryExpr $ parserConfirm parseExpr True

parserSpec = do
  putStr "\n--- Testing parsers \n"
  putStr "\n--- Symbol \n"
  quickCheck prop_SymbolRight
  quickCheck prop_SymbolLeft
  putStr "\n--- String \n"
  quickCheck prop_StringRight
  quickCheck prop_StringLeft
  quickCheck prop_StringEscape
  putStr "\n--- Atom \n"
  quickCheck prop_AtomRight
  quickCheck prop_AtomLeft
  putStr "\n--- Hashes negative \n"
  quickCheck prop_HashLeftAN
  quickCheck prop_HashLeftAtom
  putStr "\n--- Bool \n"
  quickCheck prop_BooleanRight
  putStr "\n--- Char \n"
  quickCheck prop_CharRight
  putStr "\n--- Nums \n"
  quickCheck prop_BaseNumRight
  quickCheck prop_NumRight
  quickCheck prop_NumLeft
  putStr "\n--- Quoted \n"
  quickCheck prop_QuotedRight
  putStr "\n--- Parens \n"
  quickCheck prop_ParensRight
  quickCheck prop_ParensLeft
  putStr "\n--- Expr \n"
  quickCheck prop_ExprRight
