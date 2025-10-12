module Interpreter.Data where
import Parse.AST (FunDecl, Identifier)

type Address = Int

data Value
    = VNumber Int
    | Pointer Address
    | Function FunDecl
    | Record [(Identifier, Address)]
    deriving (Show, Eq)

truthy :: Value -> Bool
truthy (VNumber i) = i /= 0
truthy (Pointer a) = a /= 0
truthy (Function _) = True
truthy (Record _) = True 
