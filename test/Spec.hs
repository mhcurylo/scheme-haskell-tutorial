import Test.QuickCheck
import Data.List (intersperse)
import Parser 
import Text.ParserCombinators.Parsec
import Control.Monad

symbols = "!#$%&|*+-/:<=>?@^_~"
alphas = ['a'..'z'] ++ ['A'..'Z']
numerics = ['0'..'9']

stringOf = listOf1 . elements 

arbitrarySymbol = stringOf symbols
arbitraryNumeric = stringOf numerics
arbitraryAlphaNumeric = stringOf (numerics ++ alphas) 
arbitraryAlphaSymbolic = stringOf (symbols ++ alphas) 
arbitraryAlphaSymbolicNumeric = stringOf (symbols ++ numerics ++ alphas) 
arbitraryString = (\a -> "\"" ++ a ++ "\"") <$> arbitraryAlphaSymbolicNumeric
arbitraryAtom = liftM2 (++) arbitraryAlphaSymbolic arbitraryAlphaSymbolicNumeric 

parserConfirm:: Parser a -> Bool -> String -> Bool
parserConfirm p b x = b == case parse p "" x of
                     Left err  -> False
                     Right val -> True
                     
parserLog:: Parser a -> Bool -> String -> Property
parserLog p i x = collect x $ parserConfirm p i x

-- show
prop_SymbolRight = forAll arbitrarySymbol $ parserConfirm symbol True
prop_SymbolLeft = forAll arbitraryAlphaNumeric $ parserConfirm symbol False

prop_StringRight = forAll arbitraryString $ parserConfirm parseString True
prop_StringLeft = forAll arbitraryAlphaSymbolicNumeric $ parserConfirm parseString False

prop_AtomRight = forAll arbitraryAtom $ parserConfirm parseAtom True
prop_AtomLeft = forAll arbitraryNumeric $ parserConfirm parseAtom False

main = do
  quickCheck prop_SymbolRight
  quickCheck prop_SymbolLeft
  quickCheck prop_StringRight
  quickCheck prop_StringLeft
  quickCheck prop_AtomRight
  quickCheck prop_AtomLeft
-- /show
