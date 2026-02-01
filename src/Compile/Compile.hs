module Compile.Compile where
import Parse.AST (Program)
import Text.Parsec (SourcePos)
import qualified IR.TacCompiler as IRCompiler
import Utils ((<$$>))
import IR.Desugar (desugar)
import Analysis.Typecheck.Type (Type)

compile :: Program (SourcePos, Type) -> Either String String
compile prog = do
    -- TODO: optimize on AST

    -- Generate IR
    let richIR = IRCompiler.compile (snd <$$> prog)

    -- TODO: optimize on IR

    -- Desugar IR to instructions supported by the underlying machine
    let rawIR = desugar <$> richIR

    -- TODO: optimize on IR

    -- TODO: register allocation

    -- Emit ASM
    Right $ ".text\n" <> show rawIR