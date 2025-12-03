module Analysis.Dataflow.Utils where
import Analysis.Cfg.Cfg (CFG(..), CFGNode(..), CFGId)
import qualified Data.Map as M
import qualified Analysis.Dataflow.Const as Const
import Data.List (intercalate)
import Text.Printf (printf)

prettyPrintConstAnalysis :: CFG a -> Const.ResultMap -> String
prettyPrintConstAnalysis (CFG idmap _root) resultMap =
    let
        nodeResults = M.toList resultMap

        nodeDescriptions = map (\(nodeId, _) ->
            maybe "???" formatNodeDescription (M.lookup nodeId idmap)
            ) nodeResults

        -- generate line given the target length
        maxLen = maximum $ length <$> nodeDescriptions
        formattedLines = zipWith (\(nodeId, resultLat) desc ->
            formatLine maxLen nodeId desc resultLat
            ) nodeResults nodeDescriptions
    in
        intercalate "\n" formattedLines

formatLine :: Int -> CFGId -> String -> Const.ResultLat -> String
formatLine maxLen nodeId nodeDesc resultLat =
    let latStr = formatResultLat resultLat
    in printf "%2d: [ %-*s ]: %s" nodeId maxLen nodeDesc latStr

-- this is the source code
formatNodeDescription :: CFGNode a -> String
formatNodeDescription (Node _ _ _ stmt) = show stmt
formatNodeDescription (FunEntry _ funName funVars _) =
    "fun entry " ++ funName ++ " (" ++ intercalate ", " funVars ++ ")"
formatNodeDescription (FunExit _ _ retVal _) =
    "return " ++ retVal ++ ";"

-- this is the list of variables
formatResultLat :: Const.ResultLat -> String
formatResultLat latMap =
    let sortedEntries = M.toList latMap
        formattedEntries = map (\(var, val) -> var ++ " -> " ++ show val) sortedEntries
    in "{ " ++ intercalate ", " formattedEntries ++ " }"
