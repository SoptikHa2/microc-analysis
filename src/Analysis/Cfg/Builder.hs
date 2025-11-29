module Analysis.Cfg.Builder where
import Analysis.Cfg.Cfg
import Parse.AST
import Control.Monad.State (State)

build :: FunDecl a -> CFG
build fun = undefined


buildStmt :: Stmt a -> State CFGMap CFGNode
