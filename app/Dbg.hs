module Dbg where

import Text.Parsec
import Parse.DeclParser (program)
import Data.Either (fromRight)
import Analysis.Typecheck.Typecheck (getTyping)
import Parse.AST (Program)
import qualified Data.Map as M
import Analysis.Typecheck.Constraints (Typeable)
import Analysis.Typecheck.Type (Type)

unwrap :: Either a b -> b
unwrap (Left _) = error "left"
unwrap (Right b) = b

ast = do
    source <- readFile "test_program.mc"
    let ast = unwrap $ parse program "test_program.mc" source
    pure ast

typeinfo = do
    unwrap . getTyping <$> ast

combine :: Program SourcePos -> M.Map (Typeable SourcePos) Type -> Program Type
combine = undefined

-- >>> typeinfo
-- fromList [(CExpr "\"test_program.mc\" (line 2, column 9)" f,Int),(CExpr "\"test_program.mc\" (line 3, column 6)" 1,Int),(CExpr "\"test_program.mc\" (line 4, column 10)" n > 0,Int),(CExpr "\"test_program.mc\" (line 4, column 10)" n,Int),(CExpr "\"test_program.mc\" (line 4, column 13)" 0,Int),(CExpr "\"test_program.mc\" (line 4, column 18)" f,Int),(CExpr "\"test_program.mc\" (line 5, column 12)" n,Int),(CExpr "\"test_program.mc\" (line 5, column 15)" n,Int),(CExpr "\"test_program.mc\" (line 5, column 8)" f * n,Int),(CExpr "\"test_program.mc\" (line 5, column 8)" f,Int),(CExpr "\"test_program.mc\" (line 6, column 12)" 1,Int),(CExpr "\"test_program.mc\" (line 6, column 8)" n - 1,Int),(CExpr "\"test_program.mc\" (line 6, column 8)" n,Int),(CExpr "\"test_program.mc\" (line 8, column 9)" f,Int),(CFun "\"test_program.mc\" (line 1, column 1)" (FunDecl {d = "test_program.mc" (line 1, column 1), name = "ite", args = ["n"], body = FunBlock {d = "test_program.mc" (line 8, column 12), idDecl = ["f"], body = [f = 1;,while (n > 0)], return = f}}),(Int) -> Int),(CId "\"test_program.mc\" (line 1, column 1)" "n",Int),(CId "ite:\"test_program.mc\" (line 1, column 1)" "f",Int)]
