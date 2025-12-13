module Dbg where

import Text.Parsec
import Parse.DeclParser (program)
import Data.Either (fromRight)
import Analysis.Typecheck.Typecheck (getTyping)

unwrap :: Either a b -> b
unwrap (Left _) = error "left"
unwrap (Right b) = b

ast = do
    source <- readFile "test_program.mc"
    let ast = unwrap $ parse program "test_program.mc" source
    pure ast

typeinfo = do
    unwrap . getTyping <$> ast
