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
    try input <|> try nul <|> try postfixExpr <?> "expression (no arithm)"

postfixExpr :: Parser (Expr SourcePos)
postfixExpr = do
    -- It certainly has a primary expression first
    e <- primaryExpr <?> "postfix into primary"
    -- Afterwards, there may be 0 or more times {field access OR call}.
    ops <- many (try fieldAccessRest <|> try callRest <|> try arrayIndex) <?> "postfix operators"

    pure $ recApply e ops
    where
        fieldAccessRest :: Parser (Expr SourcePos -> Expr SourcePos)
        fieldAccessRest = Lexer.dot >> (flip . FieldAccess <$> loc) <*> Lexer.identifierStr <?> "field access"

        callRest :: Parser (Expr SourcePos -> Expr SourcePos)
        callRest = do
            _ <- Lexer.parenOpen
            l <- loc
            ex <- many (expression <* optional Lexer.comma <* spaces) <?> "call params"
            _ <- Lexer.parenClose
            pure ((flip . Call) l) <+> ex
        
        arrayIndex :: Parser (Expr SourcePos -> Expr SourcePos)
        arrayIndex = Lexer.sqBracketOpen >> (flip . ArrayAccess <$> loc) <*> expression <* Lexer.sqBracketClose <?> "array indexing"

        -- This function has to exist
        recApply :: a -> [a -> a] -> a
        recApply a [] = a
        recApply a (x:xs) = recApply (x a) xs


primaryExpr :: Parser (Expr SourcePos)
primaryExpr = try number <|> try identifier <|> try record <|> try array <|> try paren <?> "primary expr"

identifier :: Parser (Expr SourcePos)
identifier = EIdentifier <$> loc <*> Lexer.identifierStr <?> "identifier"

record :: Parser (Expr SourcePos)
record = do
    _ <- Lexer.bracketOpen
    l <- loc
    fx <- many (field <* spaces <* optional Lexer.comma <* spaces) <?> "record fields"
    _ <- Lexer.bracketClose
    pure $ Record l (Fields fx)
    where
        field :: Parser (Identifier, Expr SourcePos)
        field = do
            i <- Lexer.identifierStr
            _ <- Lexer.colon
            e <- expression <?> "field expr"
            pure (i, e)

array :: Parser (Expr SourcePos)
array = do
    _ <- Lexer.sqBracketOpen
    l <- loc
    fx <- many (expression <* spaces <* optional Lexer.comma <* spaces) <?> "initial array elements"
    _ <- Lexer.sqBracketClose
    pure $ Array l fx

paren :: Parser (Expr SourcePos)
paren = between Lexer.parenOpen Lexer.parenClose expression <?> "parens"

number :: Parser (Expr SourcePos)
number = do
    l <- loc
    L.Number num <- Lexer.numLiteral <?> "num literal"
    pure $ Number l num

input :: Parser (Expr SourcePos)
input = Lexer.keyword L.Input >> Input <$> loc <?> "input kw"

nul :: Parser (Expr SourcePos)
nul = Lexer.keyword L.Null >> Null <$> loc <?> "null kw"

exprArithm :: Parser (Expr SourcePos)
exprArithm = do
    l <- loc
    E.buildExpressionParser (arithmTable l) arithmTerm <?> "expr arithm"

arithmTerm :: Parser (Expr SourcePos)
arithmTerm = try exprWithoutArithmetics <|> try arithmParens <?> "arithm term"

-- Chain E.Prefix (multiple derefs at once)
prefix :: Parser (Expr SourcePos -> Expr SourcePos) -> E.Operator String () Identity (Expr SourcePos)
prefix p = E.Prefix . chainl1 p $ pure (.)

arithmTable :: SourcePos -> [[E.Operator String () Identity (Expr SourcePos)]]
arithmTable l = [
        -- *, &, alloc, !; and chain
        [
            prefix $ choice [
                try $ UnOp l Deref <$ Lexer.star,
                try $ UnOp l Ref <$ Lexer.and,
                try $ UnOp l Alloc <$ Lexer.keyword L.Alloc,
                try $ UnOp l Not <$ Lexer.notOp
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
        -- comparisons (desugared to > and !)
        [
            E.Infix (try $ BiOp l Gt <$ Lexer.gt) E.AssocLeft,
            E.Infix (try $ flip (BiOp l Gt) <$ Lexer.lt) E.AssocLeft,  -- a < b -> b > a
            E.Infix (try $ (\a b -> UnOp l Not (BiOp l Gt a b)) <$ Lexer.lte) E.AssocLeft,  -- a <= b -> !(a > b)
            E.Infix (try $ (\a b -> UnOp l Not (BiOp l Gt b a)) <$ Lexer.gte) E.AssocLeft   -- a >= b -> !(b > a)
        ],
        -- equality (desugared)
        [
            E.Infix (try $ BiOp l Eq <$ Lexer.eq) E.AssocLeft,
            E.Infix (try $ (\a b -> UnOp l Not (BiOp l Eq a b)) <$ Lexer.neq) E.AssocLeft  -- a != b -> !(a == b)
        ]
    ]

arithmParens :: Parser (Expr SourcePos)
arithmParens = between Lexer.parenOpen Lexer.parenClose exprArithm <?> "arithm parens"