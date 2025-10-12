module Analysis.Utils where
import Parse.AST
import Data.Generics.Uniplate.Data
import Data.Data
import Data.List

getIdentifiersUsed :: FunBlock -> [Identifier]
getIdentifiersUsed fun = getIds fun.return <> concatMap getIds fun.body
    where
        getIds :: Data a => a -> [Identifier]
        getIds node = 
            [id | EIdentifier id <- universeBi node]

dups :: [Identifier] -> [Identifier]
dups ids = map head $ filter (\g -> length g > 1) $ group $ sort ids
