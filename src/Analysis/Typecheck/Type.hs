{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Analysis.Typecheck.Type where
import Data.List (intercalate, sortBy, sort, nub)
import Data.Data (Data)
import Data.Maybe (catMaybes)

data Type
    = Int
    | Ptr Type
    | Fun [Type] Type
    | Record [(String, Type)]
    | Array Type
    | Unknown Int -- unknown type of ID [Int] (also, free type var, but in distinct type)
    | BoundTypeVar Int
    | TypeVarBinding Int Type
    | Bottom
    deriving (Eq, Data)

instance Show Type where
  show :: Type -> String
  show Int = "Int"
  show (Ptr t) = "↑" ++ show t
  show (Fun args ret) = "(" ++ intercalate " -> " (show <$> args) ++ ") -> " ++ show ret
  show (Record args) = "{" ++ intercalate "," ((\(n,t) -> n ++ ":" ++ show t) <$> 
          (sortBy (\a b -> compare (fst a) (fst b)) args)) ++ "}"
  show (Array t) = "[" ++ show t ++ "]"
  show (Unknown i) = "?" ++ show i
  show (BoundTypeVar i) = "t" ++ show i
  show (TypeVarBinding i t) = "μ t" ++ show i ++ " . " ++ show t
  show Bottom = "◇"


type TypeError = String
