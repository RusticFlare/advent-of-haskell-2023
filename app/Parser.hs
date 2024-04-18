module Parser where

import Text.ParserCombinators.Parsec (Parser, parse)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

parseText :: Parser a -> String -> a
parseText parser text = case parse parser "" text of
    Right result -> result
    Left e -> error $ show e

lexer :: P.TokenParser st
lexer = P.makeTokenParser emptyDef

symbol :: String -> Parser String
symbol = P.symbol lexer

operator :: Parser String
operator = P.operator lexer

natural :: Parser Integer
natural = P.natural lexer

float :: Parser Double
float = either fromInteger id <$> P.naturalOrFloat lexer

identifier :: Parser String
identifier = P.identifier lexer

colon :: Parser String
colon = P.colon lexer

comma :: Parser String
comma = P.comma lexer

pipe :: Parser String
pipe = symbol "|"

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = P.semiSep lexer

parens :: Parser a -> Parser a
parens = P.parens lexer
