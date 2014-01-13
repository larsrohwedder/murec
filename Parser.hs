module Parser (
    parseFile,
    parseCommand
  ) where
  
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Data.Maybe
import Recursion

language :: LanguageDef st
language = haskellStyle 
  { reservedNames = ["MU", "R", "C", "P", ".", "import"]
  , reservedOpNames = [".", ":="] }
lexer = Token.makeTokenParser language

whiteSpace = Token.whiteSpace lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
semi = Token.semi lexer
commaSep = Token.commaSep lexer
comma = Token.comma lexer
natural = Token.natural lexer
stringLiteral = Token.stringLiteral lexer

parseFile :: String -> IO (Either ParseError SourceFile)
parseFile = parseFromFile $ do
  s <- endBy statement semi
  whiteSpace
  eof
  return s

parseCommand :: String -> Either ParseError (Function, [Int])
parseCommand = parse command ""

padded :: GenParser Char st a -> GenParser Char st a
padded m = do
  spaces
  result <- m
  spaces
  return result

braced :: GenParser Char st a -> GenParser Char st a
braced m = between (char '(') (char ')') (padded m)

parentheses :: GenParser Char st String
parentheses = between (char '"') (char '"') (many $ noneOf "\n\"")

constant :: GenParser Char st Function
constant = do
  reserved "C"
  between (char '(') (char ')') $ do
    v <- natural
    comma
    arity <- natural
    return $ Const (fromIntegral v) (fromIntegral arity)

projection :: GenParser Char st Function
projection = do
  reserved "P"
  between (char '(') (char ')') $ do
    v <- natural
    comma
    arity <- natural
    return $ Projection (fromIntegral v) (fromIntegral arity)
  
next :: GenParser Char st Function
next = do
  reserved "N"
  return Next

mu :: GenParser Char st Function
mu = do
  reserved "MU"
  f <- braced function
  return $ Mu ("", f)
    
recursive :: GenParser Char st Function
recursive = do
  reserved "R"
  braced $ do
    arg1 <- function
    comma
    arg2 <- function
    return $ Recursion ("", arg1) ("", arg2)

reference :: GenParser Char st Function
reference = do
  name <- identifier
  return $ Ref name

function :: GenParser Char st Function
function = braced f <|> f
  where
    f = do
      whiteSpace
      result <- choice [constant, next, mu, projection, recursive, reference]
      option result $ do
        reservedOp "."
        result' <- (f >>= return . (replicate 1)) 
               <|> (braced $ commaSep function)
        return $ Composition ("", result) $ map (\x -> ("", x)) result'
        
command :: GenParser Char st (Function, [Int])
command = do
  f <- function
  whiteSpace
  args <- braced $ commaSep natural
  return (f, map fromIntegral args)

statement :: GenParser Char st Statement
statement = do
  whiteSpace
  l <- (definition >>= return . (uncurry Definition))
   <|> (reserved "import" >> whiteSpace >> stringLiteral >>= return . Import)
  return l

definition :: GenParser Char st (RefName, Function)
definition = do
  name <- identifier
  reservedOp ":="
  f <- function
  return (name, f)
