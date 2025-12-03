module Analysis.Dataflow.Analysis (ResultMap, ResultLat) where
import qualified Data.Map as M
import Parse.AST (Identifier)
import Analysis.Cfg.Cfg

type ResultMap a = M.Map CFGId (ResultLat a)
type ResultLat a = M.Map Identifier a
