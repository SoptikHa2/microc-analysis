module Interpreter.InterpretRun (interpretIO) where
import Parse.AST (Program, FunDecl (..), Identifier)
import Text.Parsec (SourcePos)
import Interpreter.Data
import qualified Interpreter.State as IS
import Control.Monad.State (StateT, execStateT, evalStateT, mapStateT)
import Control.Monad.Identity
import Control.Monad
import Interpreter.Interpret (evalFun)
import System.Exit

interpretIO :: Program SourcePos -> [Int] -> IO Value
interpretIO prog args = do
    case findFunction "main" prog of
        Nothing -> do
            putStrLn "Error: No 'main' function found in program"
            exitFailure
        Just mainFun -> do
            -- Check argument count matches
            let FunDecl loc _ funArgs _ = mainFun
            let expectedArgs = length funArgs
            let providedArgs = length args
            if expectedArgs /= providedArgs
            then do
                putStrLn $ "Error: main function expects " ++ show expectedArgs
                        ++ " arguments, but " ++ show providedArgs ++ " were provided"
                exitFailure
            else do
                -- Initialize state with all functions in global scope
                initialState <- initializeState prog

                -- Run the main function with provided arguments
                evalStateT (evalFun mainFun loc (fmap VNumber args)) initialState

-- Find a function by name in the program
findFunction :: Identifier -> Program a -> Maybe (FunDecl a)
findFunction name = foldr go Nothing
  where
    go f@(FunDecl _ fname _ _) acc
      | fname == name = Just f
      | otherwise = acc

-- Initialize the interpreter state with all functions in global scope
initializeState :: Program SourcePos -> IO IS.State
initializeState prog = do
  -- Start with empty state and add all functions to global scope
  let addFunctionsAction :: StateT IS.State IO ()
      addFunctionsAction = forM_ prog $ \f@(FunDecl _ fname _ _) ->
        liftIdentityToIO $ IS.putsGlobal fname (Function f)
  execStateT addFunctionsAction IS.empty
  where
    liftIdentityToIO :: StateT IS.State Identity a -> StateT IS.State IO a
    liftIdentityToIO = mapStateT (pure . runIdentity)
