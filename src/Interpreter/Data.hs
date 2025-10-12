{-# LANGUAGE InstanceSigs #-}
module Interpreter.Data where
import Parse.AST (FunDecl(..), Identifier)

type Address = Int

data Value
    = VNumber Int
    | Pointer Address
    | Function FunDecl
    | Record [(Identifier, Address)]
    deriving (Eq)

instance Show Value where
  show :: Value -> String
  show (VNumber i) = show i
  show (Pointer addr) = "* " <> show addr
  show (Function (FunDecl name _ _)) = "<func " <> name <> ">"
  show (Record fields) = "Record " <> show (fst <$> fields)


truthy :: Value -> Bool
truthy (VNumber i) = i /= 0
truthy (Pointer a) = a /= 0
truthy (Function _) = True
truthy (Record _) = True
