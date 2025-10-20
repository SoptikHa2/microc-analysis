{-# LANGUAGE DeriveDataTypeable #-}
module Parse.AST where
import Data.Data

type Program a = [FunDecl a]

data FunDecl a = FunDecl {
        d :: a,
        name :: Identifier,
        args :: [Identifier],
        body :: FunBlock a
    }
    deriving (Show, Eq, Data, Typeable)

type Identifier = String

data FunBlock a = FunBlock {
        d :: a,
        idDecl :: [Identifier],
        body :: [Stmt a],
        return :: Expr a
    }
    deriving (Show, Eq, Data, Typeable)

data Stmt a
    = OutputStmt a (Expr a)
    | WhileStmt {
        d :: a,
        condition :: Expr a,
        body :: Stmt a
    }
    | IfStmt {
        d :: a,
        condition :: Expr a,
        body :: Stmt a,
        elseBody :: Maybe (Stmt a)
    }
    | Block a [Stmt a]
    | AssignmentStmt a (Expr a) (Expr a)
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

data Expr a
    = BiOp a BiOp (Expr a) (Expr a)
    | UnOp a UnOp (Expr a)
    | Input a
    | Null a
    | FieldAccess a (Expr a) Identifier
    | Call {
        d :: a,
        target :: Expr a,
        args   :: [Expr a]
    }
    | Record a (Record a)
    | Number a Int
    | EIdentifier a Identifier
    deriving (Show, Eq, Data, Typeable)

newtype Record a = Fields [(Identifier, Expr a)]
    deriving (Show, Eq, Data, Typeable)
