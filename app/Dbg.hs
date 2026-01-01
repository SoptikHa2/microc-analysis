{-# LANGUAGE TupleSections #-}
module Dbg where

import Text.Parsec
import Parse.DeclParser (program)
import Data.Either (fromRight)
import Analysis.Typecheck.Typecheck (getTyping)
import Parse.AST (Program, Expr)
import qualified Data.Map as M
import Analysis.Typecheck.Constraints (Typeable(CExpr))
import Analysis.Typecheck.Type (Type (Bottom))
import IR.TacCompiler (compile)
import Utils ((<$$>))
import Data.Generics.Uniplate.Data

unwrap :: Either a b -> b
unwrap (Left _) = error "left"
unwrap (Right b) = b

ast = do
    source <- readFile "test_program.mc"
    let ast = unwrap $ parse program "test_program.mc" source
    pure ast

typeinfo = do
    unwrap . getTyping <$> ast

combine :: Program SourcePos -> M.Map (Typeable SourcePos) Type -> Program (SourcePos, Type)
combine ast typemap = astWithType
    where
        astWithBotType = (, Bottom) <$$> ast :: Program (SourcePos, Type)
        astWithType = transformBi (annotateExpr typemap) astWithBotType

        annotateExpr :: M.Map (Typeable SourcePos) Type -> Expr (SourcePos, Type) -> Expr (SourcePos, Type)
        annotateExpr typingmap expr = (\(s,_) -> (s,dstType)) <$> expr
            where
                dstType = typingmap M.! CExpr (fst <$> expr)
-- >>> typeinfo
-- fromList [(CExpr "\"test_program.mc\" (line 2, column 9)" f,Int),(CExpr "\"test_program.mc\" (line 3, column 10)" 2,Int),(CExpr "\"test_program.mc\" (line 3, column 6)" 1 + 2,Int),(CExpr "\"test_program.mc\" (line 3, column 6)" 1,Int),(CExpr "\"test_program.mc\" (line 4, column 9)" f,Int),(CFun "\"test_program.mc\" (line 1, column 1)" (FunDecl {d = "test_program.mc" (line 1, column 1), name = "main", args = [], body = FunBlock {d = "test_program.mc" (line 4, column 12), idDecl = ["f"], body = [f = 1 + 2;], return = f}}),() -> Int),(CId "main:\"test_program.mc\" (line 1, column 1)" "f",Int)]

-- >>> compile <$> ast

