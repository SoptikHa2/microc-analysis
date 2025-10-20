module Lex.Comments (comment) where
import Text.Parsec.String
import Text.Parsec

singleLineComment :: Parser ()
singleLineComment =
    string "//" >> manyTill anyChar newline >> return ()

blockComment :: Parser ()
blockComment =
    string "/*" >> manyTill anyChar (string "*/") >> return ()

comment :: Parser ()
comment = spaces >> (try singleLineComment <|> try blockComment) >> spaces >> return ()
