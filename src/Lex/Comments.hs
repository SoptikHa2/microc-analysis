module Lex.Comments (comment) where
import Text.Parsec.String
import Text.Parsec

singleLineComment :: Parser ()
singleLineComment =
    string "//" >> manyTill anyChar newline >> spaces >> return ()

blockComment :: Parser ()
blockComment =
    string "/*" >> manyTill anyChar (string "*/") >> spaces >> return ()

comment :: Parser ()
comment = spaces >> many (try singleLineComment <|> try blockComment) >> spaces >> return ()
