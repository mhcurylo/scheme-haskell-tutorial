import Test.QuickCheck
import Data.List (intersperse)
import Parser (symbol)
import Text.ParserCombinators.Parsec

arbitarySymbol = sublistOf "!#$%&|*+-/:<=>?@^_~"
arbitaryAZ = listOf $ elements ['a'..'z']

parserConfirm:: Parser a -> String -> Bool
parserConfirm p x = case (parse p "" x) of
                     Left err  -> False
                     Right val -> True

-- show
prop_SymbolRight = forAll arbitarySymbol $ parserConfirm symbol
prop_SymbolLeft = forAll arbitaryAZ $ not . parserConfirm symbol

main = do
  quickCheck prop_SymbolRight
  quickCheck prop_SymbolLeft
-- /show
