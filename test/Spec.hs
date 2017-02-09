import Test.QuickCheck
import Data.List (intersperse)
import Parser 
import Text.ParserCombinators.Parsec

arbitrarySymbol = sublistOf "!#$%&|*+-/:<=>?@^_~"
arbitraryAlpha = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']
arbitraryNumeric = listOf $ elements ['0'..'9']
arbitraryAlphaNumeric = oneof [arbitraryAlpha, arbitraryNumeric]
arbitraryAlphaNumericSymbol = oneof [arbitraryAlphaNumeric, arbitrarySymbol]
arbitraryChars:: Gen String 
arbitraryChars = arbitrary
arbitraryString = (\a -> "\"" ++ a ++ "\"") <$> arbitraryAlphaNumericSymbol 


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
prop_StringLeft = forAll arbitraryAlphaNumeric $ parserConfirm parseString False


main = do
  quickCheck prop_SymbolRight
  quickCheck prop_SymbolLeft
  quickCheck prop_StringRight
  quickCheck prop_StringLeft
-- /show
