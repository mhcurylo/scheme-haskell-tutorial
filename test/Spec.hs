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
arbitraryAlphaNumeric = stringOf symbols
arbitraryString = (\a -> "\"" ++ a ++ "\"") <$> stringOf (symbols ++ alphas ++ numerics) 


parserConfirm:: Parser a -> Bool -> String -> Bool
parserConfirm p b x = b == case parse p "" x of
                     Left err  -> False
                     Right val -> True
                     
parserLog:: Parser a -> Bool -> String -> Property
parserLog p i x = collect x $ parserConfirm p i x

-- show
prop_SymbolRight = forAll arbitrarySymbol $ parserConfirm symbol True
prop_SymbolLeft = forAll arbitraryAlphaNumeric $ parserLog symbol False

prop_StringRight = forAll arbitraryString $ parserConfirm parseString True
prop_StringLeft = forAll arbitraryAlphaNumeric $ parserConfirm parseString False


prop_AtomRight = forAll arbitrarySymbol $ parserConfirm parseAtom True
prop_AtomLeft = forAll arbitraryNumeric $ parserConfirm parseAtom False

main = do
  sample arbitrarySymbol
  quickCheck prop_SymbolRight
  quickCheck prop_SymbolLeft
  quickCheck prop_StringRight
  quickCheck prop_StringLeft
  quickCheck prop_AtomRight
  quickCheck prop_AtomLeft
-- /show
