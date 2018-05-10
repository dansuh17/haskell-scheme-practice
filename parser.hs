import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- algebraic data type
-- each are constructors
data LispVal = Atom String  -- String naming the atom
             | List [LispVal]  -- proper list
             | DottedList [LispVal] LispVal  -- imporper list from scheme : (a b . c)
             | Number Integer
             | String String
             | Character String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- use parser action skipMany1, to get a Parser that recognize one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- here, bind means "attempt to match the first parser,
-- then attempt to match the second with the remaining input, and faile if either fails.
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

-- parse scheme character, having forms #\<character>
parseCharacter :: Parser LispVal
parseCharacter = do
  char '#'
  char '\\'
  x <- many (noneOf " ")
  return $ String x

-- find and return escape sequences
escapes :: Parser Char
escapes = do
  char '\\'   -- find '\' symbol - starting the escape character
  c <- oneOf "\"nrt\\"  -- find \", \n, \r, \t, \\
  return c

-- use >> if actions don't return a value
-- >>= if you'll be immediately passing that value into the next action
-- 'do' otherwise
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> escapes)  -- retrieve until another " symbol
  char '"'
  return $ String x  -- construct a LispVal

-- <|> is the choice operator : tries the first parser,
-- then if it fails, tries the second
parseAtom:: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- result of many1 digit has type "Parser String", so we lift the (Number . read)
-- to a monad to be able to operate on it
-- liftM :: Monad m => (a -> b) -> m a -> m b
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- using do-notation
parseNumber' :: Parser LispVal
parseNumber' = do
  digits <- many1 digit
  return $ Number (read digits)

-- using >>=
parseNumber'' :: Parser LispVal
parseNumber'' = (many1 digit) >>= (\digits -> return $ Number (read digits))

-- parse list (like (1 2 3 5 6) in scheme)
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- dotted-list parser
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- single-quote syntactic sugar of Scheme
parseQuoted :: ParserLispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- accepts either string, number, or an atom
parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber'
  <|> parseQuoted
  <|> do char '('
         x <- try parseList <|> parseDottedList  -- backtracking in parsec
         char ')'
         return x

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
