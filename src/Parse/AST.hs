{-# LANGUAGE DuplicateRecordFields #-}

module Parse.AST where

type Program = [FunDecl]

data FunDecl = FunDecl {
        name :: Identifier,
        args :: [Identifier],
        body :: FunBlock
    }

type Identifier = String

data FunBlock = FunBlock {
        idDecl :: [Identifier],
        body :: [Stmt],
        return :: Expr
    }

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

data UnOp
    = Deref
    | Ref
    | Alloc

data BiOp
    = Eq
    | Gt
    | Plus
    | Minus
    | Mul
    | Div

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
    | Record {
        fields :: [(Identifier, Expr)]
    }
