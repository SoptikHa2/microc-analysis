{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Analysis.Typecheck.Type (Type(..), TypeError, sizeof, innerSizeOf, isSimpleType) where
import Data.List (intercalate, sortBy)
import Data.Data (Data)

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
    deriving (Eq, Data, Ord)

-- How many memory cells does a type take after compiling
sizeof :: Type -> Int
sizeof Int = 1
sizeof (Ptr _) = 1
sizeof (Fun _ _) = 1
sizeof (Record fx) = 1
sizeof (Array _) = 1
sizeof (Unknown _) = error "Cannot determine size of unknown type. Does it have finite size?"
sizeof (BoundTypeVar _) = error "Type does not have finite size."
sizeof t'@(TypeVarBinding _ t) = error $ "Type " <> show t' <> " does not have finite size."
sizeof Bottom = 0

-- Ie. record is stored as ptr, but this return the real size of the data
innerSizeOf :: Type -> Int
innerSizeOf (Record fx) = sum $ sizeof . snd <$> fx
innerSizeOf (Array t) = error "Cannot determine array length, we don't know how many elements does it have."
innerSizeOf t = sizeof t

-- Whether the type is something that can be handled via simple register operations (so no useless indirection builtin)
isSimpleType :: Type -> Bool
isSimpleType Int = True
isSimpleType (Ptr _) = True
isSimpleType (Fun _ _) = True  -- still function pointer
isSimpleType (Record _) = False  -- we need to handle storing it somewhere... painful
isSimpleType (Array _) = False
isSimpleType t = False

instance Show Type where
  show :: Type -> String
  show Int = "Int"
  show (Ptr t) = "↑" ++ show t
  show (Fun args ret) = "(" ++ intercalate " -> " (show <$> args) ++ ") -> " ++ show ret
  show (Record args) = "{" ++ intercalate "," ((\(n,t) -> n ++ ":" ++ show t) <$>
          sortBy (\a b -> compare (fst a) (fst b)) args) ++ "}"
  show (Array t) = "[" ++ show t ++ "]"
  show (Unknown i) = "?" ++ show i
  show (BoundTypeVar i) = "t" ++ show i
  show (TypeVarBinding i t) = "μ t" ++ show i ++ " . " ++ show t
  show Bottom = "◇"


type TypeError = String
