module Parse.DeclParser (program, func, varStmt, retStmt) where

import qualified Lex.Lexer as Lexer
import qualified Lex.Tokens as L
import Parse.AST hiding (name, args, body)
import Text.Parsec
import Text.Parsec.String (Parser)
import Parse.ExprParser (expression)
import Parse.StmtParser (stmt)
import Utils
import Parse.Location

program :: Parser (Program SourcePos)
program = many func <?> "program"

func :: Parser (FunDecl SourcePos)
func = do
    l <- loc
    name <- Lexer.identifierStr <?> "fun name"
    _ <- Lexer.parenOpen
    args <- many (Lexer.identifierStr <* optional Lexer.comma) <?> "fun args"
    _ <- Lexer.parenClose
    _ <- Lexer.bracketOpen
    vars <- concat <$> many (try varStmt) <?> "fun var declares"
    bodyx <- many (try stmt) <?> "fun body"
    ret  <- retStmt <?> "fun return"
    body <- FunBlock <$> loc <+> vars <+> bodyx <+> ret
    _ <- Lexer.bracketClose
    pure $ FunDecl l name args body

varStmt :: Parser [Identifier]
varStmt = do
    _ <- Lexer.keyword L.Var
    idx <- many1 (Lexer.identifierStr <* optional Lexer.comma) <?> "var declaration"
    _ <- Lexer.semicolon
    pure idx

retStmt :: Parser (Expr SourcePos)
retStmt = Lexer.keyword L.Return *> expression <* Lexer.semicolon <?> "ret statement"
