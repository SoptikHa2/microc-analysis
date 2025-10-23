module Analysis.Error (Error(..)) where
import Analysis.Semantics (SemanticError)
import Analysis.Typecheck.Type

data Error 
    = Semantic SemanticError
    | Type TypeError


instance Show Error where
    show (Semantic e) = show e
    show (Type e) = show e
