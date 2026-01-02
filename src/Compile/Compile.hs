module Compile.Compile where
import Parse.AST (Program)
import Analysis.Typecheck.Typecheck (getTyping, typeAST)
import Text.Parsec (SourcePos)
import qualified IR.TacCompiler as IRCompiler
import Utils ((<$$>))
import IR.Desugar (desugar)

compile :: Program SourcePos -> Either String String
compile prog = do
    typing <- getTyping prog

    -- TODO: optimize on AST

    -- Generate IR
    let progWithTypes = typeAST prog typing
    let richIR = IRCompiler.compile (snd <$$> progWithTypes)

    -- TODO: optimize on IR

    -- Desugar IR to instructions supported by the underlying machine
    let rawIR = desugar <$> richIR

    -- TODO: optimize on IR

    -- TODO: register allocation

    -- Emit ASM
    Right $ ".text\n" <> show rawIR