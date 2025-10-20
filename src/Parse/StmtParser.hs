module Parse.StmtParser (stmt) where

import qualified Lex.Lexer as Lexer
import qualified Lex.Tokens as L
import Parse.AST
import Text.Parsec
import Text.Parsec.String (Parser)
import Parse.ExprParser (expression)
import Parse.Location
import Utils

stmt :: Parser (Stmt SourcePos)
stmt = try outputStmt <|> try whileStmt <|> try ifStmt <|> try blockStmt <|> try assStmt <?> "stmt"

outputStmt :: Parser (Stmt SourcePos)
outputStmt = OutputStmt <$> loc <*> (Lexer.keyword L.Output *> expression <* Lexer.semicolon) <?> "output stmt"

whileStmt :: Parser (Stmt SourcePos)
whileStmt = 
    WhileStmt <$> loc <*> (Lexer.keyword L.While *> Lexer.parenOpen *> expression <* Lexer.parenClose) <*> stmt <?> "while stmt"

ifStmt :: Parser (Stmt SourcePos)
ifStmt = do
    _ <- Lexer.keyword L.If *> Lexer.parenOpen
    cond <- expression <?> "if cond"
    _ <- Lexer.parenClose
    body <- stmt <?> "if body"
    elseBody <- optionMaybe $ try $ Lexer.keyword L.Else *> stmt
    IfStmt <$> loc <+> cond <+> body <+> elseBody

blockStmt :: Parser (Stmt SourcePos)
blockStmt =
    Block <$> loc <*> (Lexer.bracketOpen *> many stmt <* Lexer.bracketClose) <?> "block"

assStmt :: Parser (Stmt SourcePos)
assStmt = AssignmentStmt <$> loc <*> expression <* Lexer.assign <*> expression <* Lexer.semicolon <?> "assignment"
