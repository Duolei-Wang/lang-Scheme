module R5RS.Evaluator
  ( eval,
    showTypedVal,
  )
where

import Control.Monad.Error
import Data.IORef
import Data.Maybe
import R5RS.Parser

showTypedVal :: LispVal -> String
showTypedVal lsval = case lsval of
  Atom s -> "Atom " ++ s
  Number i -> "Number " ++ show i
  Bool b -> "Bool " ++ show b
  String str -> "String" ++ str
  List lst -> "List (" ++ unwords (map showTypedVal lst) ++ ")"
  Quoted x -> "Quoted" ++ showTypedVal x
  DottedList dlst tail -> "DList (" ++ unwords (map showTypedVal dlst) ++ " . " ++ showTypedVal tail ++ ")"
  (PrimitiveFunc _) -> "<primitive>"
  (Func {params = args, vararg = varargs, body = body, closure = env}) ->
    "(lambda ("
      ++ unwords (map show args)
      ++ ( case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ ") ...)"

showVal :: LispVal -> [Char]
showVal lsval = case lsval of
  Atom s -> s
  Number i -> show i
  Bool b -> show b
  String str -> str
  List lst -> "(" ++ unwords (map showTypedVal lst) ++ ")"
  Quoted x -> "\'" ++ showTypedVal x
  DottedList dlst tail -> "(" ++ unwords (map showTypedVal dlst) ++ " . " ++ showTypedVal tail ++ ")"

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then 0
        else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

apply' :: String -> [LispVal] -> LispVal
apply' fn args = maybe (Bool False) ($ args) $ lookup fn primitives

bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
  do env <- liftIO $ readIORef envRef
    maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
    return
    value

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom func : args)) =
  mapM (eval env) args >>= liftThrows . apply func
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply fn args = case fn of
  (PrimitiveFunc func) -> liftThrows $ func args
  (Func params varargs body closure) ->
    if num params /= num args && isNothing varargs
      then throwError $ NumArgs (num params) args
      else
        liftIO (bindVars closure $ zip params args)
          >>= bindVarArgs varargs
          >>= evalBody
    where
      remainingArgs = drop (length params) args
      num = toInteger . length
      evalBody env = last <$> mapM (eval env) body
      bindVarArgs arg env = case arg of
        Just argName ->
          liftIO $
            bindVars
              env
              [ ( argName,
                  List remainingArgs
                )
              ]
        Nothing -> return env
