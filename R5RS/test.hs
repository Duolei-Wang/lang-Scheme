import R5RS.Evaluator
  ( eval,
    showTypedVal,
  )
import R5RS.Parser
  ( LispVal (String),
    parseExpr,
  )
import Text.ParserCombinators.Parsec (parse)

instance Show LispVal where
  show :: LispVal -> String
  show = showTypedVal

showExpr :: String -> String
showExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found:\n" ++ show val

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

main :: IO ()
main = do
  print $ readExpr "\'atom"
  print $ readExpr "2"
  print $ readExpr "\"a string\""
  print $ eval $ readExpr "(+ 2 2)"
  putStrLn "(END)"