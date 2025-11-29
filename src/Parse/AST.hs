{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Parse.AST where
import Data.Data
import Data.List (intercalate)

type Program a = [FunDecl a]

data FunDecl a = FunDecl {
        d :: a,
        name :: Identifier,
        args :: [Identifier],
        body :: FunBlock a
    }
    deriving (Show, Eq, Ord, Data, Typeable)

type Identifier = String

data FunBlock a = FunBlock {
        d :: a,
        idDecl :: [Identifier],
        body :: [Stmt a],
        return :: Expr a
    }
    deriving (Show, Eq, Ord, Data, Typeable)

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
    deriving (Eq, Ord, Data, Typeable)

data UnOp
    = Deref
    | Ref
    | Alloc
    deriving (Eq, Ord, Data, Typeable)

data BiOp
    = Eq
    | Gt
    | Plus
    | Minus
    | Mul
    | Div
    deriving (Eq, Ord, Data, Typeable)

data Expr a
    = BiOp a BiOp (Expr a) (Expr a)
    | UnOp a UnOp (Expr a)
    | Input a
    | Null a
    | FieldAccess a (Expr a) Identifier
    | ArrayAccess {
        d :: a,
        target :: Expr a,
        idx :: Expr a
    }
    | Call {
        d :: a,
        target :: Expr a,
        args   :: [Expr a]
    }
    | Record a (Record a)
    | Array a [Expr a]
    | Number a Int
    | EIdentifier a Identifier
    deriving (Eq, Ord, Data, Typeable)

newtype Record a = Fields [(Identifier, Expr a)]
    deriving (Eq, Ord, Data, Typeable)

exprLoc :: Expr a -> a
exprLoc (BiOp l _ _ _) = l
exprLoc (UnOp l _ _) = l
exprLoc (Input l) = l
exprLoc (Null l) = l
exprLoc (FieldAccess l _ _) = l
exprLoc (ArrayAccess l _ _) = l
exprLoc (Call l _ _) = l
exprLoc (Parse.AST.Record l _) = l
exprLoc (Array l _) = l
exprLoc (Number l _) = l
exprLoc (EIdentifier l _) = l

instance Show UnOp where
    show Deref = "*"
    show Ref = "&"
    show Alloc = "alloc"

instance Show BiOp where
    show Eq = "=="
    show Gt = ">"
    show Plus = "+"
    show Minus = "-"
    show Mul = "*"
    show Div = "/"

instance Show (Stmt a) where
    show (OutputStmt _ e) = "output " <> show e
    show (WhileStmt _ c b) = "while(" <> show c <> ") " <> show b
    show (IfStmt _ c t f) = "if(" <> show c <> ") " <> show t <> show (fstr f)
        where
            fstr Nothing = ""
            fstr (Just fb) = " else " <> show fb
    show (Block _ stmx) = "{\n" <> intercalate "\n" (show <$> stmx) <> "\n}"
    show (AssignmentStmt _ l r) = show l <> " = " <> show r

instance Show (Expr a) where
    show (BiOp _ o l r) = show l <> " " <> show o <> " " <> show r
    show (UnOp _ u e) = show u <> " " <> show e
    show (Input _) = "input"
    show (Null _) = "null"
    show (FieldAccess _ t f) = show t <> "." <> show f
    show (ArrayAccess _ t i) = show t <> "[" <> show i <> "]"
    show (Call _ t ax) = show t <> "(" <> intercalate "," (show <$> ax) <> ")"
    show (Record _ rx) = show rx
    show (Array _ ex) = "[" <> intercalate ", " (show <$> ex) <> "]"
    show (Number _ i) = show i
    show (EIdentifier _ i) = i

instance Show (Record a) where
    show (Fields fx) = "{" <> intercalate ", " (sf <$> fx) <> "}"
        where
            sf (i, e) = show i <> ": " <> show e
