module Parse.ExprParser (expression) where

import qualified Lex.Lexer as Lexer
import qualified Lex.Tokens as L
import Parse.AST
import Text.Parsec
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)
import Parse.Location
import Utils
import Control.Monad.Identity (Identity)


expression :: Parser (Expr SourcePos)
expression = try exprArithm <|> try exprWithoutArithmetics <?> "expression"

exprWithoutArithmetics :: Parser (Expr SourcePos)
exprWithoutArithmetics =
    try input <|> try nul <|> try postfixExpr

postfixExpr :: Parser (Expr SourcePos)
postfixExpr = do
    -- It certainly has a primary expression first
    e <- primaryExpr
    -- Afterwards, there may be 0 or more times {field access OR call}.
    ops <- many (try fieldAccessRest <|> try callRest)

    pure $ recApply e ops
    where
        fieldAccessRest :: Parser (Expr SourcePos -> Expr SourcePos)
        fieldAccessRest = Lexer.dot >> (flip . FieldAccess <$> loc) <*> Lexer.identifierStr

        callRest :: Parser (Expr SourcePos -> Expr SourcePos)
        callRest = do
            _ <- Lexer.parenOpen
            ex <- many (expression <* optional Lexer.comma <* spaces)
            _ <- Lexer.parenClose
            (flip . Call <$> loc) <+> ex

        -- This function has to exist
        recApply :: a -> [a -> a] -> a
        recApply a [] = a
        recApply a (x:xs) = recApply (x a) xs


primaryExpr :: Parser (Expr SourcePos)
primaryExpr = try number <|> try identifier <|> try record <|> try paren

identifier :: Parser (Expr SourcePos)
identifier = EIdentifier <$> loc <*> Lexer.identifierStr

record :: Parser (Expr SourcePos)
record = do
    _ <- Lexer.bracketOpen
    fx <- many (field <* spaces <* optional Lexer.comma <* spaces)
    _ <- Lexer.bracketClose
    Record <$> loc <+> Fields fx
    where
        field :: Parser (Identifier, Expr SourcePos)
        field = do
            i <- Lexer.identifierStr
            _ <- Lexer.colon
            e <- expression
            pure (i, e)

paren :: Parser (Expr SourcePos)
paren = between Lexer.parenOpen Lexer.parenClose expression

number :: Parser (Expr SourcePos)
number = do
    L.Number num <- Lexer.numLiteral
    Number <$> loc <+> num

input :: Parser (Expr SourcePos)
input = Lexer.keyword L.Input >> Input <$> loc

nul :: Parser (Expr SourcePos)
nul = Lexer.keyword L.Null >> Null <$> loc

exprArithm :: Parser (Expr SourcePos)
exprArithm = do
    l <- loc
    E.buildExpressionParser (arithmTable l) arithmTerm

arithmTerm :: Parser (Expr SourcePos)
arithmTerm = try exprWithoutArithmetics <|> try arithmParens

-- Chain E.Prefix (multiple derefs at once)
prefix :: Parser (Expr SourcePos -> Expr SourcePos) -> E.Operator String () Identity (Expr SourcePos)
prefix p = E.Prefix . chainl1 p $ pure (.)

arithmTable :: SourcePos -> [[E.Operator String () Identity (Expr SourcePos)]]
arithmTable l = [
        -- *, &, alloc; and chain
        [
            prefix $ choice [
                try $ UnOp l Deref <$ Lexer.star,
                try $ UnOp l Ref <$ Lexer.and,
                try $ UnOp l Alloc <$ Lexer.keyword L.Alloc
            ]
        ],
        -- _ * _, /
        [
            E.Infix (try $ BiOp l Mul <$ Lexer.star) E.AssocLeft,
            E.Infix (try $ BiOp l Div <$ Lexer.division) E.AssocLeft
        ],
        -- +, -
        [
            E.Infix (try $ BiOp l Plus <$ Lexer.plus) E.AssocLeft,
            E.Infix (try $ BiOp l Minus <$ Lexer.minus) E.AssocLeft
        ],
        -- <, ==
        [
            E.Infix (try $ BiOp l Gt <$ Lexer.gt) E.AssocLeft
        ],
        [
            E.Infix (try $ BiOp l Eq <$ Lexer.eq) E.AssocLeft
        ]
    ]

arithmParens :: Parser (Expr SourcePos)
arithmParens = between Lexer.parenOpen Lexer.parenClose exprArithm