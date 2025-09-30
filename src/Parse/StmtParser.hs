module Parse.StmtParser (stmt) where

import qualified Lex.Lexer as Lexer
import qualified Lex.Tokens as L
import Parse.AST
import Text.Parsec
import Text.Parsec.String (Parser)
import Parse.ExprParser (expression)

stmt :: Parser Stmt
stmt = try outputStmt <|> try whileStmt <|> try ifStmt <|> try blockStmt <|> try assStmt

outputStmt :: Parser Stmt
outputStmt = OutputStmt <$> (Lexer.keyword L.Output *> expression <* Lexer.semicolon)

whileStmt :: Parser Stmt
whileStmt = 
    WhileStmt <$> (Lexer.keyword L.While *> Lexer.parenOpen *> expression <* Lexer.parenClose) <*> stmt

ifStmt :: Parser Stmt
ifStmt = do
    _ <- Lexer.keyword L.If *> Lexer.parenOpen
    cond <- expression
    _ <- Lexer.parenClose
    body <- stmt
    elseBody <- optionMaybe $ try $ Lexer.keyword L.Else *> stmt
    pure $ IfStmt cond body elseBody

blockStmt :: Parser Stmt
blockStmt =
    Block <$> (Lexer.bracketOpen *> many stmt <* Lexer.bracketClose)

assStmt :: Parser Stmt
assStmt = AssignmentStmt <$> expression <* Lexer.assign <*> expression <* Lexer.semicolon
