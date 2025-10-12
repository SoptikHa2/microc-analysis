module Interpreter.InterpretStmt where
import Control.Monad.State (StateT)
import Interpreter.State (State(..))
import Parse.AST (Stmt)

interpretStmt :: Stmt -> StateT State IO ()
interpretStmt = undefined
