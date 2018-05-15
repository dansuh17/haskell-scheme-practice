import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except --- built-in error functions (Either)

-- algebraic data type
-- each are constructors
data LispVal = Atom String  -- String naming the atom
             | List [LispVal]  -- proper list
             | DottedList [LispVal] LispVal  -- imporper list from scheme : (a b . c)
             | Number Integer
             | String String
             | Character String
             | Bool Bool

-- lisp error represented
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
showError (Parser parseErr) = "Parse error at " ++ show parseErr

-- make it an instance of Show typeclass
instance Show LispError where show = showError

-- curried type constructors  -- can also be partially applied
type ThrowsError = Either LispError

-- pattern matching : destructures algebraic data types and selects code clause based on constructor
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List as) = "(" ++ unwordsList as ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- instance of Show typeclass = datatype LispVal having 'show' function
instance Show LispVal where show = showVal

-- unwords glues together list of words with spaces
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal  -- point-free style

-- symbols in Lisp
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- use parser action skipMany1, to get a Parser that recognize one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- here, bind means "attempt to match the first parser,
-- then attempt to match the second with the remaining input, and faile if either fails.
{-
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found " ++ show val
-}

-- new readExpr that returns a value
{-
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
-}

-- error-handling version
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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
parseQuoted :: Parser LispVal
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

{-
eval :: LispVal -> LispVal -- Lisp has save types for both code and data
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
-- apply function to a list of evaluated args
-- for example, (+ 2 2) would be applying [Number 2, Number 2] to "+"
eval (List (Atom func : args)) = apply func $ map eval args
-}

-- eval now returns 'Either LispError LispVal'
-- throwError
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do result <- eval pred
     case result of
          Bool False -> eval alt
          otherwise -> eval conseq
-- mapM maps a monadic function over a list of values,
-- sequences the resulting actions together with bind, and returns the list of inner results
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
-- throwError :: e -> m a

-- maybe returns (Bool False) if not found, or applies ($ args) to return value of the last argument
-- lookup looks for keys in a list of pairs
{-
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
-}

-- error throwing version
-- now maybe returns a LispError if not found, or applies ($ args) to return value
-- of the last argument
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args)
        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("symbol?", checkType "symbol"),
  ("string?", checkType "string"),
  ("number?", checkType "number"),
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
  ("string>=?", strBoolBinop (>=))]

checkType :: String -> [LispVal] -> ThrowsError LispVal
checkType "string" [(String _)] = return $ Bool True
checkType "symbol" [(Atom _)] = return $ Bool True
checkType "number" [(Number _)] = return $ Bool True
checkType _ _ = return $ Bool False

-- foldl does not restrict number of params to 2
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do left <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- convert LispVal to Integer
{-
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                         if null parsed then 0 else fst $ parsed !! 0  -- error
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0  -- error
-}

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
  if null parsed
    then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- this is like 'head' in Haskell
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList  -- Left

-- similar to 'tail' in Haskell
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


{-
main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
-}

{-
main :: IO ()
main = getArgs >>= print . eval . readExpr . head
-}

-- catchError takes an Either and a function that turns an error into Either.
-- if the action represents an error, it applies the function
-- catchError :: m a -> (e -> m a) -> m a
trapError action = catchError action (return . show)

-- intentionally undefine Left constructor pattern - represents programmer error
-- we intend to use extractVAlue only after catchError
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main :: IO ()
main = do
  args <- getArgs
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
  -- >>= has higher precedence than $
  -- takes the first argument, parse it to eval, call show, and return to make it an IO action
  -- evaled :: ThrowsError String
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
