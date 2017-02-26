module Lib
    ( interpreter,
    ) where

import Evaluator
import Errors
import Parser
import System.Environment
import System.IO

interpreter :: IO ()
interpreter = do args <- getArgs
                 case length args of
                   0 -> runRepl 
                   1 -> evalAndPrint $ head args
                   _ -> putStrLn "Program works takes one or no lines"
     

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalStr :: String -> IO String
evalStr expr = return $ extractValue $ trapError (fmap show $ readExpr expr  >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalStr expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

