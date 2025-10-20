{-# LANGUAGE ScopedTypeVariables #-}
module TestUtils (normalizeSourcePos) where

import Data.Data
import Data.Generics.Uniplate.Data (transformBi)
import Text.Parsec.Pos (SourcePos, newPos)

-- Normalize all SourcePos values to a dummy position for testing
-- This allows tests to compare ASTs without worrying about actual source positions
normalizeSourcePos :: forall a. Data a => a -> a
normalizeSourcePos = transformBi (\(_ :: SourcePos) -> newPos "test" 0 0)
