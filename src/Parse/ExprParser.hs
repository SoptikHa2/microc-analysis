module Parse.ExprParser (expression) where

import qualified Lex.Lexer as Lexer
import qualified Lex.Tokens as L
import Parse.AST
import Text.Parsec
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)


expression :: Parser Expr
expression = try exprArithm <|> try exprWithoutArithmetics <?> "expression"

exprWithoutArithmetics :: Parser Expr
exprWithoutArithmetics =
    try input <|> try nul <|> try alloc <|> try postfixExpr

postfixExpr :: Parser Expr
postfixExpr = do
    -- It certainly has a primary expression first
    e <- primaryExpr
    -- Afterwards, there may be 0 or more times {field access OR call}.
    ops <- many (try fieldAccessRest <|> try callRest)

    pure $ recApply e ops
    where
        fieldAccessRest :: Parser (Expr -> Expr)
        fieldAccessRest = Lexer.dot >> flip FieldAccess <$> Lexer.identifierStr

        callRest :: Parser (Expr -> Expr)
        callRest = do
            _ <- Lexer.parenOpen
            ex <- many (expression <* spaces <* optional (char ',') <* spaces)
            _ <- Lexer.parenClose
            pure $ flip Call ex

        -- This function has to exist
        recApply :: a -> [a -> a] -> a
        recApply a [] = a
        recApply a (x:xs) = recApply (x a) xs


primaryExpr :: Parser Expr
primaryExpr = try number <|> try identifier <|> try record <|> try paren

identifier :: Parser Expr
identifier = EIdentifier <$> Lexer.identifierStr

record :: Parser Expr
record = do
    _ <- Lexer.bracketOpen
    fx <- many (field <* spaces <* optional (char ',') <* spaces)
    _ <- Lexer.bracketClose
    pure $ Record $ Fields fx
    where
        field :: Parser (Identifier, Expr)
        field = do
            i <- Lexer.identifierStr
            _ <- Lexer.colon
            e <- expression
            pure (i, e)

paren :: Parser Expr
paren = between Lexer.parenOpen Lexer.parenClose expression

number :: Parser Expr
number = do
    L.Number num <- Lexer.numLiteral
    pure $ Number num

input :: Parser Expr
input = Lexer.keyword L.Input >> pure Input

nul :: Parser Expr
nul = Lexer.keyword L.Null >> pure Null

alloc :: Parser Expr
alloc = Lexer.keyword L.Alloc >> UnOp Alloc <$> expression

exprArithm :: Parser Expr
exprArithm = E.buildExpressionParser arithmTable arithmTerm

arithmTerm :: Parser Expr
arithmTerm = try exprWithoutArithmetics <|> try arithmParens

arithmTable = [
        [
            E.Prefix (try $ UnOp Deref <$ Lexer.star)
        ],
        [
            E.Prefix (try $ UnOp Ref <$ Lexer.and) -- &
        ],
        [
            E.Prefix (try $ UnOp Alloc <$ Lexer.keyword L.Alloc)
        ],
        [
            E.Infix (try $ BiOp Mul <$ Lexer.star) E.AssocLeft,
            E.Infix (try $ BiOp Div <$ Lexer.division) E.AssocLeft
        ],
        [
            E.Infix (try $ BiOp Plus <$ Lexer.plus) E.AssocLeft,
            E.Infix (try $ BiOp Minus <$ Lexer.minus) E.AssocLeft
        ],
        [
            E.Infix (try $ BiOp Gt <$ Lexer.gt) E.AssocLeft
        ],
        [
            E.Infix (try $ BiOp Eq <$ Lexer.eq) E.AssocLeft
        ]
    ]

arithmParens :: Parser Expr
arithmParens = between Lexer.parenOpen Lexer.parenClose exprArithm