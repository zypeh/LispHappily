module Main where

import           Text.Megaparsec hiding (manyTill, many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Control.Applicative.Combinators
import           Control.Monad.Combinators.Expr
import           Data.Void

main :: IO ()
main = putStrLn "Hello world"

type Name = String
data Term
    = App Term Term
    | Lam Name Term
    | Lit Literal
    deriving (Show)

data Literal
    = LInt Integer
    | LChar Char
    | LString String
    | LBool Bool
    | LVar String
    deriving (Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = Lexer.space space1 (Lexer.skipLineComment ";;") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: String -> Parser String
symbol = Lexer.symbol sc

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

integer :: Parser Integer
integer = lexeme Lexer.decimal

charLiteral :: Parser Char
charLiteral  = between (char '\'') (char '\'') Lexer.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill Lexer.charLiteral (char '\"')

bool :: Parser Bool
bool = (string "#t" *> return True)
   <|> (string "#f" *> return False)

reservedKeyword :: [String]
reservedKeyword = ["lambda", "if"]

identifier :: Parser String
identifier = do
    ident <- many alphaNumChar

    if elem ident reservedKeyword
        then return empty
        else return ident

literalParse :: Parser Literal
literalParse = 
        LInt <$> integer 
    <|> LChar <$> charLiteral
    <|> LString <$> stringLiteral
    <|> LBool <$> bool
    <|> LVar <$> many alphaNumChar

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where
    rest x = do { f <- op; y <- p; rest (f x y) }
         <|> return x

appParse :: Parser Term
appParse = parens $ termParse `chainl1` (char ' ' *> return App)

type Env = [(Name, Term)]

-- (lambda (.) . (.))
lambdaParse :: Parser Term
lambdaParse = do
    symbol "lambda"
    name <- many alphaNumChar
    term <- termParse
    return $ Lam name term

applicationParse :: Parser (Term -> Term)
applicationParse = do
  terms <- many termParse
  return $ \fn -> do
    let app arg = App arg
    foldl app fn terms

termParse :: Parser Term
termParse = lambdaParse
        <|> Lit <$> literalParse
        <|> appParse
