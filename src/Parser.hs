module Parser where
import           Control.Monad
import           Data.Bool
import           Expr
import           Text.Parsec.Number
import           Text.ParserCombinators.Parsec

parseExpr = parseFrac


runParseExpr = parse parseExpr "Expression parser"

parseFrac = chainl1 parseAS $ do
  spaces
  char '%'
  spaces
  pure Quotient



parseAS :: Parser Expr
parseAS = chainl1 parseMD $ do
  spaces
  op <- oneOf "+-"
  spaces
  pure $ case op of
    '+' -> Sum
    '-' -> \a b -> Sum a (Product (Const (-1)) b)

parseMD = chainl1 parseExp $ do
  spaces
  op <- oneOf "*/"
  spaces
  pure $ case op of
    '*' -> Product
    '/' -> Quotient

parseExp = chainl1 parseUnit $ do
  spaces
  char '^'
  spaces
  pure Pow

parseUnit = do
  spaces
  res <- parseNum <|> parseX <|> parseP <|> parseE <|> ap parseNeg parseUnit <|> ap parseLn parseUnit
  spaces
  pure res

parseX = char 'x' >> pure X

parseE = char 'e' >> pure (Const e)

parseNeg = do
  char '-'
  spaces
  pure (Product (Const (-1)))

parseLn = do
  string "ln"
  spaces
  pure Ln

parseP = do
  char '('
  spaces
  expr <- parseExpr
  char ')'
  spaces
  pure expr

parseNum = Const <$> floating3 False
