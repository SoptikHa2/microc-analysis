module Analysis.Dataflow.Utils (prettyPrintAnalysis) where
import Analysis.Cfg.Cfg (CFG(..), CFGNode(..), CFGId)
import qualified Data.Map as M
import Analysis.Dataflow.Analysis
import Data.List (intercalate)
import Text.Printf (printf)

prettyPrintAnalysis :: Show l => CFG a -> ResultMap (ResultLat l) -> String
prettyPrintAnalysis (CFG nodeMap _root) resultMap =
    let
        nodeResults = M.toList resultMap

        nodeDescriptions = map (\(nodeId, _) ->
            maybe "???" formatNodeDescription (M.lookup nodeId nodeMap)
            ) nodeResults

        -- generate line given the target length
        maxLen = maximum $ length <$> nodeDescriptions
        formattedLines = zipWith (\(nodeId, resultLat) desc ->
            formatLine maxLen nodeId desc resultLat
            ) nodeResults nodeDescriptions
    in
        intercalate "\n" formattedLines

formatLine :: Show l => Int -> CFGId -> String -> ResultLat l -> String
formatLine maxLen nodeId nodeDesc resultLat =
    let latStr = formatResultLat resultLat
    in printf "%2d: [ %-*s ]: %s" nodeId maxLen nodeDesc latStr

-- this is the source code
formatNodeDescription :: CFGNode a -> String
formatNodeDescription (Node _ _ _ stmt) = show stmt
formatNodeDescription (FunEntry _ funName funVars funArgs _) =
    "fun entry " ++ funName ++ " (" ++ intercalate ", " funArgs ++ ")" ++ vars
    where
        vars = if null funVars then "" else " ; var " ++ intercalate ", " funVars
formatNodeDescription (FunExit _ _ retVal _) =
    "return " ++ show retVal ++ ";"

-- this is the list of variables
formatResultLat :: Show l => ResultLat l -> String
formatResultLat latMap =
    let sortedEntries = M.toList latMap
        formattedEntries = map (\(var, val) -> var ++ " -> " ++ show val) sortedEntries
    in "{ " ++ intercalate ", " formattedEntries ++ " }"
