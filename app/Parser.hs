module Parser where

import Text.ParserCombinators.Parsec (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer :: P.TokenParser st
lexer = P.makeTokenParser emptyDef

symbol :: String -> Parser String
symbol = P.symbol lexer

natural :: Parser Integer
natural = P.natural lexer

colon :: Parser String
colon = P.colon lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = P.semiSep lexer
