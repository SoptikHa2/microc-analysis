module Lex.Lexer where

import Text.Parsec
import Lex.Tokens (Token(..), Keyword, kwToStr)
import Lex.Comments (comment)

ws :: Parsec String () ()
ws = try comment <|> spaces

parenOpen :: Parsec String () Token
parenOpen = ws >> char '(' >> return ParenOpen

parenClose :: Parsec String () Token
parenClose = ws >> char ')' >> return ParenClose

identifierStr :: Parsec String () String
identifierStr = try $ do
    ws
    idFirst <- letter <|> char '_'
    idRest <- many (alphaNum <|> char '_')
    pure $ idFirst : idRest

identifier :: Parsec String () Token
identifier = Identifier <$> identifierStr

bracketOpen :: Parsec String () Token
bracketOpen = ws >> char '{' >> return CBracketOpen

bracketClose :: Parsec String () Token
bracketClose = ws >> char '}' >> return CBracketClose

keyword :: Keyword -> Parsec String () Token
keyword kw = try $ do
    ws
    let valid = kwToStr kw
    _ <- choice (try . string <$> valid)
    pure $ Keyword kw

semicolon :: Parsec String () Token
semicolon = ws >> char ';' >> return Semicolon

comma :: Parsec String () Token
comma = ws >> char ',' >> return Comma

assign :: Parsec String () Token
assign = ws >> char '=' >> return Assign

plus :: Parsec String () Token
plus = ws >> char '+' >> return Plus

minus :: Parsec String () Token
minus = ws >> char '-' >> return Minus

star :: Parsec String () Token
star = ws >> char '*' >> return Star

division :: Parsec String () Token
division = ws >> char '/' >> return Division

and :: Parsec String () Token
and = ws >> char '&' >> return And

dot :: Parsec String () Token
dot = ws >> char '.' >> return Dot

colon :: Parsec String () Token
colon = ws >> char ':' >> return Colon

gt :: Parsec String () Token
gt = ws >> char '>' >> return Gt

eq :: Parsec String () Token
eq = ws >> string "==" >> return Eq

numLiteral :: Parsec String () Token
numLiteral = do
    ws
    minusSign <- optionMaybe $ try $ char '-'
    num <- try (read <$> many1 digit)
    case minusSign of
        Just _ -> pure $ Number (- num)
        Nothing -> pure $ Number num
