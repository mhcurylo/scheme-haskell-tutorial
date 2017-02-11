import Test.QuickCheck
import Data.List (intersperse)
import Parser 
import Text.ParserCombinators.Parsec
import Control.Monad

symbols = "!#$%&|*+-/:<=>?@^_~"
alphas = ['a'..'z'] ++ ['A'..'Z']
numerics = ['0'..'9']
escapes = "\"\\0nrvtbf"

stringOf = listOf1 . elements 

arbitrarySymbol = stringOf symbols
arbitraryNumeric = stringOf numerics
arbitraryAlphaNumeric = stringOf (numerics ++ alphas) 
arbitraryAlphaSymbolic = stringOf (symbols ++ alphas) 
arbitraryAlphaSymbolicNumeric = stringOf (symbols ++ numerics ++ alphas) 
arbitraryString = (\a -> "\"" ++ a ++ "\"") <$> arbitraryAlphaSymbolicNumeric
arbitraryAtom = liftM2 (++) arbitraryAlphaSymbolic arbitraryAlphaSymbolicNumeric 
arbitraryEscape = (\a -> ['\\', a]) <$> elements escapes
arbitraryStringWithEscape = (\a -> "\"" ++ a ++ "\"") <$> liftM2 (++) arbitraryAlphaSymbolicNumeric arbitraryEscape
arbitraryBool = (\a -> ['#', a]) <$> elements "tf"
arbitraryChar = oneof [elements ["space", "newline"], arbitrarySingleChar] 
arbitrarySingleChar = liftM2 (\a b -> ['#', a, b]) (elements "a") (elements " ()")

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


schemeBool "#t" = Bool True
schemeBool "#f" = Bool False
schemeChar "#space" = Character ' '
schemeChar "#newline" = Character '\n'
schemeChar (a:b:c) = Character b
schemeString = String . tail . init

-- show
prop_SymbolRight = forAll arbitrarySymbol $ parserEq symbol head
prop_SymbolLeft = forAll arbitraryAlphaNumeric $ parserConfirm symbol False

prop_StringRight = forAll arbitraryString $ parserEq parseString schemeString
prop_StringLeft = forAll arbitraryAlphaSymbolicNumeric $ parserConfirm parseString False
prop_StringEscape = forAll arbitraryStringWithEscape $ parserEq parseString schemeString

prop_AtomRight = forAll arbitraryAtom $ parserEq parseAtom Atom
prop_AtomLeft = forAll arbitraryNumeric $ parserConfirm parseAtom False

prop_BooleanRight = forAll arbitraryBool $ parserEq parseHash schemeBool
prop_BooleanLeft = forAll arbitraryAlphaNumeric $ parserConfirm parseHash False 

prop_CharRight = forAll arbitraryBool $ parserEq parseHash schemeChar
prop_CharLeft = forAll arbitraryAlphaNumeric $ parserConfirm parseHash False 

main = do
  putStr "\n--- Testing parsers \n"
  quickCheck prop_SymbolRight
  quickCheck prop_SymbolLeft
  quickCheck prop_StringRight
  quickCheck prop_StringLeft
  quickCheck prop_StringEscape
  quickCheck prop_AtomRight
  quickCheck prop_AtomLeft
-- /show
