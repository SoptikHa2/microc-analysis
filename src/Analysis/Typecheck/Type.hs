module Analysis.Typecheck.Type where

data Type
    = Int
    | Ptr Type
    | Fun [Type] Type
    | Record [(String, Type)]
    | Unknown Int -- unknown type of ID [Int]
    deriving (Show, Eq)

type TypeError = String
