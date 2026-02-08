{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveFunctor #-}
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
    deriving (Show, Eq, Ord, Data, Typeable, Functor)

type Identifier = String

data FunBlock a = FunBlock {
        d :: a,
        idDecl :: [Identifier],
        body :: [Stmt a],
        return :: Expr a
    }
    deriving (Show, Eq, Ord, Data, Typeable, Functor)

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
    deriving (Eq, Ord, Data, Typeable, Functor)

data UnOp
    = Deref
    | Ref
    | Alloc
    | Not
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
    deriving (Eq, Ord, Data, Typeable, Functor)

newtype Record a = Fields [(Identifier, Expr a)]
    deriving (Eq, Ord, Data, Typeable, Functor)

exprData :: Expr a -> a
exprData (BiOp l _ _ _) = l
exprData (UnOp l _ _) = l
exprData (Input l) = l
exprData (Null l) = l
exprData (FieldAccess l _ _) = l
exprData (ArrayAccess l _ _) = l
exprData (Call l _ _) = l
exprData (Parse.AST.Record l _) = l
exprData (Array l _) = l
exprData (Number l _) = l
exprData (EIdentifier l _) = l

-- Set only the top-level annotation without recursing into sub-expressions
setExprData :: a -> Expr a -> Expr a
setExprData d (BiOp _ op l r) = BiOp d op l r
setExprData d (UnOp _ op e) = UnOp d op e
setExprData d (Input _) = Input d
setExprData d (Null _) = Null d
setExprData d (FieldAccess _ e f) = FieldAccess d e f
setExprData d (ArrayAccess _ t i) = ArrayAccess d t i
setExprData d (Call _ t a) = Call d t a
setExprData d (Parse.AST.Record _ r) = Parse.AST.Record d r
setExprData d (Array _ e) = Array d e
setExprData d (Number _ i) = Number d i
setExprData d (EIdentifier _ i) = EIdentifier d i

instance Show UnOp where
    show Deref = "*"
    show Ref = "&"
    show Alloc = "alloc"
    show Not = "!"

instance Show BiOp where
    show Eq = "=="
    show Gt = ">"
    show Plus = "+"
    show Minus = "-"
    show Mul = "*"
    show Div = "/"

instance Show (Stmt a) where
    show (OutputStmt _ e) = "output " <> show e <> ";"
    show (WhileStmt _ c _b) = "while (" <> show c <> ")" 
    show (IfStmt _ c _t _f) = "if (" <> show c <> ")" 
    show (Block _ stmx) = "{\n" <> intercalate "\n" (show <$> stmx) <> "\n}"
    show (AssignmentStmt _ l r) = show l <> " = " <> show r <> ";"

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
