module Workflow (SourceData(..), AnalysisData(..), AnyError(..), getSource, getAna) where
import Parse.AST (Program, FunDecl (name), Identifier)
import Text.Parsec (SourcePos, parse)
import Analysis.Typecheck.Type (Type, TypeError)
import Analysis.Dataflow.ReachingDef (ReachingDefResultMap)
import Analysis.Dataflow.Const (ConstResultMap)
import Analysis.Cfg.Cfg (CFG)
import Analysis.Dataflow.VeryBusy (VeryBusyResultMap)
import Text.Parsec.Error (ParseError)
import Analysis.Semantics (SemanticError, verifyM)
import Parse.DeclParser (program)
import Analysis.Typecheck.Typecheck (getTyping, typeAST)
import Utils ((<!>))
import qualified Analysis.Cfg.Builder as CFGBuilder
import qualified Analysis.Dataflow.Const as ConstAna
import qualified Data.Map as M
import qualified Analysis.Dataflow.VeryBusy as VeryBusyAna
import qualified Analysis.Dataflow.ReachingDef as ReachingDefsAna
import Data.List (intercalate, zipWith4)
import Analysis.Typecheck.Constraints (Typeable)

data SourceData = SourceData {
    ast :: Program (SourcePos, Type),
    typing :: M.Map (Typeable SourcePos) Type
}

data AnalysisData = AnalysisData {
    cfg :: CFG (SourcePos, Type),
    consts :: ConstResultMap,
    reachingDefs :: ReachingDefResultMap (SourcePos, Type),
    veryBusy ::  VeryBusyResultMap (SourcePos, Type)
}

data AnyError
    = Parsing ParseError
    | Check [SemanticError]
    | Typing TypeError

instance Show AnyError where
    show (Parsing pe) = show pe
    show (Check sex) = intercalate "\n" (show <$> sex)
    show (Typing te) = show te

getSource :: String -> FilePath -> Either AnyError SourceData
getSource sourceCode filePath = do
    ast      <- parse program filePath sourceCode  <!> Parsing
    typeinfo <- getTyping ast                      <!> Typing
    _        <- Analysis.Semantics.verifyM ast     <!> Check
    pure $ SourceData (typeAST ast typeinfo) typeinfo

getAna :: (Program (SourcePos, Type)) -> M.Map Identifier AnalysisData
getAna ast =
    let
        cfg = CFGBuilder.build <$> ast
        const = ConstAna.solve <$> cfg
        rdefs = ReachingDefsAna.solve <$> cfg
        vbusy = VeryBusyAna.solve <$> cfg
    in
        M.fromList $ zip (name <$> ast) (zipWith4 AnalysisData cfg const rdefs vbusy)
