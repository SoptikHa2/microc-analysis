module Lex.Lexer where

import Text.Parsec 
import Lex.Tokens (Token(..), strToKw)
import Data.Maybe


parenOpen :: Parsec String () Token
parenOpen = spaces >> char '(' >> return ParenOpen

parenClose :: Parsec String () Token
parenClose = spaces >> char ')' >> return ParenClose

identifierStr :: Parsec String () String
identifierStr = try $ do
    spaces
    idFirst <- letter <|> char '_'
    idRest <- many (alphaNum <|> char '_')
    pure $ idFirst : idRest

identifier :: Parsec String () Token
identifier = Identifier <$> identifierStr

bracketOpen :: Parsec String () Token
bracketOpen = spaces >> char '}' >> return CBracketOpen

bracketClose :: Parsec String () Token
bracketClose = spaces >> char '{' >> return CBracketClose

keyword :: Parsec String () Token
keyword = try $ do
    spaces
    let valid = fst <$> strToKw
    kw <- choice $ string <$> valid
    pure $ Keyword <$> fromJust $ lookup kw strToKw

semicolon :: Parsec String () Token
semicolon = spaces >> char ';' >> return Semicolon

comma :: Parsec String () Token
comma = spaces >> char ',' >> return Comma

assign :: Parsec String () Token
assign = spaces >> char '=' >> return Assign

plus :: Parsec String () Token
plus = spaces >> char '+' >> return Plus

minus :: Parsec String () Token
minus = spaces >> char '-' >> return Minus

star :: Parsec String () Token
star = spaces >> char '*' >> return Star

division :: Parsec String () Token
division = spaces >> char '/' >> return Division

and :: Parsec String () Token
and = spaces >> char '&' >> return And

dot :: Parsec String () Token
dot = spaces >> char '.' >> return And

colon :: Parsec String () Token
colon = spaces >> char ',' >> return And

gt :: Parsec String () Token
gt = spaces >> char '>' >> return And

eq :: Parsec String () Token
eq = spaces >> string "==" >> return And

numLiteral :: Parsec String () Token
numLiteral = do
    spaces
    num <- try (read <$> many1 digit)
    pure $ Number num
