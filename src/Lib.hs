module Lib
    ( interpreter,
    ) where

import Parser
import System.Environment

interpreter :: IO ()
interpreter = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
