module Compile.Compile where
import Parse.AST (Program)

compile :: Program a -> Either String String
compile prog = Left "not impl"