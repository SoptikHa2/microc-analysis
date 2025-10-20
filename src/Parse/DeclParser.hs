module Parse.DeclParser where

import qualified Lex.Lexer as Lexer
import qualified Lex.Tokens as L
import Parse.AST
import Text.Parsec
import Text.Parsec.String (Parser)
import Parse.ExprParser (expression)
import Parse.StmtParser (stmt)
import Utils
import Parse.Location

program :: Parser (Program SourcePos)
program = many func

func :: Parser (FunDecl SourcePos)
func = do
    l <- loc
    name <- Lexer.identifierStr
    _ <- Lexer.parenOpen
    args <- many (Lexer.identifierStr <* optional Lexer.comma)
    _ <- Lexer.parenClose
    _ <- Lexer.bracketOpen
    vars <- concat <$> many (try varStmt)
    bodyx <- many (try stmt)
    ret  <- retStmt
    body <- FunBlock <$> loc <+> vars <+> bodyx <+> ret
    _ <- Lexer.bracketClose
    pure $ FunDecl l name args body

varStmt :: Parser [Identifier]
varStmt = do
    _ <- Lexer.keyword L.Var
    idx <- many1 $ Lexer.identifierStr <* optional Lexer.comma
    _ <- Lexer.semicolon
    pure idx

retStmt :: Parser (Expr SourcePos)
retStmt = Lexer.keyword L.Return *> expression <* Lexer.semicolon
