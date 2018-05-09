import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- each are constructors
data LispVal = Atom String  -- String naming the atom
             | List [LispVal]  -- proper list
             | DottedList [LispVal] LispVal  -- imporper list from scheme : (a b . c)
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- use parser action skipMany1, to get a Parser that recognize one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- here, bind means "attempt to match the first parser,
-- then attempt to match the second with the remaining input, and faile if either fails.
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

-- use >> if actions don't return a value
-- >>= if you'll be immediately passing that value into the next action
-- 'do' otherwise
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
