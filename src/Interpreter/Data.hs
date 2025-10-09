module Interpreter.Data where
import Parse.AST (FunDecl, Record)

type Address = Int

data Value
    = VNumber Int
    | Pointer Address
    | Function FunDecl
    | Record Record
    deriving (Show, Eq)
