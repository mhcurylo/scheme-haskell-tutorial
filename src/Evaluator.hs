{-# LANGUAGE ExistentialQuantification #-}

module Evaluator (
    eval,
    primitiveBindings
) where

import           Control.Monad.Except
import           Environment
import           LispVal
import           Parser
import           System.IO


eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Atom "'nil") = return val
eval env (Atom id') = getVar env id'
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pre, conseq, alt]) =
     do result <- eval env pre
        case result of
          Bool False -> eval env alt
          Bool True  -> eval env conseq
          _          -> throwError $ TypeMismatch "Bool" result
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) = makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) = makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) = makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) = makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) = makeVarArgs varargs env [] body'
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List (Atom "case": condition: cases)) = lispCase env condition cases
eval env (List (func : args)) = do func' <- eval env func
                                   argVals <- mapM (eval env) args
                                   apply func' argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

lispCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
lispCase env _ [] = evalAllReturnLast env []
lispCase env _ [List (Atom "else" : args)] = evalAllReturnLast env args
lispCase env condition (List (List datums: exprs) : xs) = do key <- eval env condition
                                                             eqvs <- mapM (\x -> liftThrows $ eqv [key, x]) datums
                                                             if Bool True `elem` eqvs
                                                                then evalAllReturnLast env exprs
                                                                else lispCase env key xs
lispCase _ condition _ = throwError $ BadSpecialForm "Bad form of case" $ List [condition]

evalAllReturnLast :: Env -> [LispVal] -> IOThrowsError LispVal
evalAllReturnLast env [] = eval env $ Atom "'nil"
evalAllReturnLast env l@(_:_) = mapM (eval env) l >>= return . last


makeFunc varargs env params' body' = return $ Func (map show params') varargs body' env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = fmap last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
              Nothing -> return env
apply _ _ = throwError $ Default "Tired to misapply"

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string?", isType "String"),
              ("symbol?", isType "Atom"),
              ("number?", isType "Number"),
              ("boolean?", isType "Bool"),
              ("list?", isType "List"),
              ("number?", isType "Number"),
              ("character?", isType "Char"),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

ioPrimitives ::[(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars (map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives))
                 where makeFunc constructor (var, func) = (var, constructor func)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = fmap (Number . foldl1 op) (mapM unpackNum params)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
              then throwError $ NumArgs 2 args
              else do left <- unpacker $ head args
                      right <- unpacker $ args !! 1
                      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

isType :: String -> [LispVal] -> ThrowsError LispVal
isType t args = if l < 1 || l > 1
  then throwError $ NumArgs 1 args
  else return $ Bool $ case head args of
    (Atom _)         -> t == "Atom"
    (List _)         -> t == "List"
    (DottedList _ _) -> t == "List"
    (Number _)       -> t == "Number"
    (String _)       -> t == "String"
    (Bool _)         -> t == "Bool"
    (Character _)    -> t == "Char"
    _                -> False
  where l = length args

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]              = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]          = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]          = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]              = return $ Bool $ arg1 == arg2
eqv dl@[DottedList _ _, DottedList _ _] = eqLispList eqv dl
eqv l@[List _, List _]                  = eqLispList eqv l
eqv [_, _]                              = return $ Bool False
eqv badArgList                          = throwError $ NumArgs 2 badArgList


eqLispList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqLispList f [DottedList xs x, DottedList ys y] = eqLispList f [List $ xs ++ [x], List $ ys ++ [y]]
eqLispList f [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
        where eqvPair (x1, x2) = case f [x1, x2] of
                Left err         -> False
                Right (Bool val) -> val

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
                                                  unpacked1 <- unpacker arg1
                                                  unpacked2 <- unpacker arg2
                                                  return $ unpacked1 == unpacked2
                                                `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal dl@[DottedList _ _, DottedList _ _]  = eqLispList equal dl
equal l@[List _, List _] = eqLispList equal l
equal [arg1, arg2]       = do
                             primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                                                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                             eqvEquals <- eqv [arg1, arg2]
                             return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
