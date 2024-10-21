module R5RS.Evaluator
  ( eval,
    showTypedVal,
  )
where

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

apply :: Maybe ([LispVal] -> LispVal) -> [LispVal] -> LispVal
apply mfn args = case mfn of
  Nothing -> Bool False
  Just fn -> fn args

eval :: LispVal -> LispVal
eval lspval = case lspval of
  Quoted val -> val
  (List (Atom fn : args)) -> apply (lookup fn primitives) $ map eval args
  expr -> expr
