module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.Functor.Identity (Identity)

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellDef

symbol :: String -> Parser String
symbol = P.symbol lexer

natural :: Parser Integer
natural = P.natural lexer
