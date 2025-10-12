module Analysis.Error (Error(..)) where
import Analysis.Semantics (SemanticError)

data Error = Semantic SemanticError
    deriving (Show)