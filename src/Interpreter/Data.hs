{-# LANGUAGE InstanceSigs #-}
module Interpreter.Data where
import Parse.AST (FunDecl(..), Identifier)
import Text.Parsec (SourcePos)

type Address = Int

data Value
    = VNumber Int
    | Pointer Address
    | Function (FunDecl SourcePos)
    | Record [(Identifier, Address)]
    deriving (Eq)

instance Show Value where
  show :: Value -> String
  show (VNumber i) = show i
  show (Pointer addr) = "<ptr " <> show addr <> ">"
  show (Function (FunDecl loc name _ _)) = "<func " <> name <> " (" <> show loc <> ")>"
  show (Record fields) = "Record " <> show (fst <$> fields)


truthy :: Value -> Bool
truthy (VNumber i) = i /= 0
truthy (Pointer a) = a /= 0
truthy (Function _) = True
truthy (Record _) = True
