{-# LANGUAGE InstanceSigs #-}
module Analysis.Typecheck.Type where
import Data.List (intercalate)

data Type
    = Int
    | Ptr Type
    | Fun [Type] Type
    | Record [(String, Type)]
    | Unknown Int -- unknown type of ID [Int]
    deriving (Eq)

instance Show Type where
  show :: Type -> String
  show Int = "Int"
  show (Ptr t) = "<Ptr " ++ (show t) ++ ">"
  show (Fun args ret) = intercalate " -> " (show <$> args) ++ " -> " ++ show ret
  show (Record args) = "{ " ++ show args ++ " }"
  show (Unknown _) = "?"


type TypeError = String
