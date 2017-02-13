module Lib
    ( interpreter,
    ) where

import Evaluator
import Parser
import System.Environment

interpreter :: IO ()
interpreter = getArgs >>= print . eval . readExpr . head
