module Compile.Optimization.AST.Optimize (optimize) where
import Text.Parsec (SourcePos)
import Analysis.Typecheck.Type (Type)
import Parse.AST (Program, FunDecl (name))
import Workflow (getAna)
import qualified Data.Map as M
import qualified Compile.Optimization.AST.ConstPropagation as ConstProp

optimize :: Program (SourcePos, Type) -> Program (SourcePos, Type)
optimize prog
    | prog == prog' = prog
    | otherwise     = optimize prog'
    where
        adata = getAna prog
        prog' = optimizeFun <$> prog
        optimizeFun fun = case M.lookup fun.name adata of
            Just ad -> ConstProp.optimize ad fun
            Nothing -> fun
