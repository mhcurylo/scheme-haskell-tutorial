module Lib
    ( interpreter,
    ) where

import Evaluator
import Errors
import Parser
import System.Environment

interpreter :: IO ()
interpreter = do
    args <- getArgs
    evaled <- return $ fmap show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
