{-# LANGUAGE InstanceSigs #-}
module Error (MicroCError(..)) where
import Data.Data
import Control.Exception (Exception)

data MicroCError
    = EInterpreter String
    | ESemanticAnalysis String
    deriving (Typeable)

instance Show MicroCError where
  show :: MicroCError -> String
  show (EInterpreter reason) = "microc: Error during interpretation.\n" <> reason
  show (ESemanticAnalysis reason) = "microc: Semantic issue found.\n" <> reason


instance Exception MicroCError
