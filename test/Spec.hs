import Test.QuickCheck
import Data.List (intersperse)
import Parser (symbol)
import Text.ParserCombinators.Parsec

arbitarySymbol = sublistOf "!#$%&|*+-/:<=>?@^_~"
arbitaryAZ = listOf $ elements ['a'..'z']

parserConfirm:: Parser a -> Bool -> String -> Bool
parserConfirm p b x = b == case parse p "" x of
                     Left err  -> False
                     Right val -> True
                     
parserLog:: Parser a -> Bool -> String -> Property
parserLog p i x = collect x $ parserConfirm p i x

-- show
prop_SymbolRight = forAll arbitarySymbol $ parserConfirm symbol True
prop_SymbolLeft = forAll arbitaryAZ $ parserLog symbol False

main = do
  quickCheck prop_SymbolRight
  quickCheck prop_SymbolLeft
-- /show
