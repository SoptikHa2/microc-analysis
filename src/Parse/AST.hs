{-# LANGUAGE DuplicateRecordFields #-}

module Parse.AST where

type Program = [FunDecl]

data FunDecl = FunDecl {
        name :: Identifier,
        args :: [Identifier],
        body :: FunBlock
    }
    deriving (Show, Eq)

type Identifier = String

data FunBlock = FunBlock {
        idDecl :: [Identifier],
        body :: [Stmt],
        return :: Expr
    }
    deriving (Show, Eq)

data Stmt
    = OutputStmt Expr
    | WhileStmt {
        condition :: Expr,
        body :: Stmt
    }
    | IfStmt { 
        condition :: Expr,
        body :: Stmt,
        elseBody :: Maybe Stmt
    }
    | Block [Stmt]
    | AssignmentStmt Expr Expr
    deriving (Show, Eq)

data UnOp
    = Deref
    | Ref
    | Alloc
    deriving (Show, Eq)

data BiOp
    = Eq
    | Gt
    | Plus
    | Minus
    | Mul
    | Div
    deriving (Show, Eq)

data Expr
    = BiOp BiOp Expr Expr
    | UnOp UnOp Expr
    | Input
    | Null
    | FieldAccess Expr Identifier
    | Call {
        target :: Expr,
        args   :: [Expr]
    }
    | Record Record
    | Number Int
    | EIdentifier Identifier
    deriving (Show, Eq)

newtype Record = Fields [(Identifier, Expr)]
    deriving (Show, Eq)
