{-# LANGUAGE DeriveDataTypeable #-}
module Parse.AST where
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data

type Program = [FunDecl]

data FunDecl = FunDecl {
        name :: Identifier,
        args :: [Identifier],
        body :: FunBlock
    }
    deriving (Show, Eq, Data, Typeable)

type Identifier = String

data FunBlock = FunBlock {
        idDecl :: [Identifier],
        body :: [Stmt],
        return :: Expr
    }
    deriving (Show, Eq, Data, Typeable)

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
    deriving (Show, Eq, Data, Typeable)

data UnOp
    = Deref
    | Ref
    | Alloc
    deriving (Show, Eq, Data, Typeable)

data BiOp
    = Eq
    | Gt
    | Plus
    | Minus
    | Mul
    | Div
    deriving (Show, Eq, Data, Typeable)

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
    deriving (Show, Eq, Data, Typeable)

newtype Record = Fields [(Identifier, Expr)]
    deriving (Show, Eq, Data, Typeable)
