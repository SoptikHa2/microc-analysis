module Parse.DeclParser where

import qualified Lex.Lexer as Lexer
import qualified Lex.Tokens as L
import Parse.AST
import Text.Parsec
import Text.Parsec.String (Parser)
import Parse.ExprParser (expression)
import Parse.StmtParser (stmt)

program :: Parser Program
program = many func

func :: Parser FunDecl
func = do
    name <- Lexer.identifierStr
    _ <- Lexer.parenOpen
    args <- many (Lexer.identifierStr <* optional Lexer.comma)
    _ <- Lexer.parenClose
    _ <- Lexer.bracketOpen
    vars <- concat <$> many (try varStmt)
    bodyx <- many (try stmt)
    ret  <- retStmt
    let body = FunBlock vars bodyx ret
    _ <- Lexer.bracketClose
    pure $ FunDecl name args body

varStmt :: Parser [Identifier]
varStmt = do
    _ <- Lexer.keyword L.Var
    idx <- many1 $ Lexer.identifierStr <* optional Lexer.comma
    _ <- Lexer.semicolon
    pure idx

retStmt :: Parser Expr
retStmt = Lexer.keyword L.Return *> expression <* Lexer.semicolon
