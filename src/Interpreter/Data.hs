module Interpreter.Data where
import Parse.AST (FunDecl, Identifier)

type Address = Int

data Value
    = VNumber Int
    | Pointer Address
    | Function FunDecl
    | Record [(Identifier, Address)]
    deriving (Show, Eq)
