module IR.TacPrint where
import IR.Tac
import Data.List (intercalate)
import Analysis.Typecheck.Type
import IR.CompilerState (Reg(..))

instance Show Reg where
    show BP = "BP"
    show SP = "SP"
    show (R n) = "R" <> show n

instance Show AnySource where
    show (Register r) = show r
    show (Imm i) = show i

instance Show AnyTarget where
    show (Direct s) = show s
    show (Deref s 0) = "[" <> show s <> "]"
    show (Deref s offset) 
        | offset > 0 = "[" <> show s <> " + " <> show offset <> "]"
        | otherwise = "[" <> show s <> " + " <> show offset <> "]"  -- Already has minus sign

showBinInstr :: (Show l, Show r) => String -> Type -> l -> r -> String
showBinInstr op t l r = op <> " " <> show l <> ", " <> show r <> " # " <> show t

instance Show TinyCInstr where
    show (Add t l r) = showBinInstr "ADD" t (Register l) r
    show (Sub t l r) = showBinInstr "SUB" t (Register l) r
    show (And t l r) = showBinInstr "AND" t (Register l) r
    show (Mul t l r) = showBinInstr "MUL" t (Register l) r
    show (Xor t l r) = showBinInstr "XOR" t (Register l) r
    show (Or t l r)  = showBinInstr "OR"  t (Register l) r
    show (Not t r) = "NOT " <> show (Register r) <> " # " <> show t
    show (Div t l r) = showBinInstr "DIV" t (Register l) r
    show (Mov t l r) = showBinInstr "MOV" t l r
    show (Lea t l r) = showBinInstr "LEA" t (Register l) r
    show (Jmp label) = "JMP " <> show label
    show (Jz reg label) = "JZ " <> show label <> ", " <> show (Register reg)
    show (Push val) = "PUSH " <> show val
    show (Pop t r) = "POP " <> show (Register r) <> " # " <> show t
    show Halt = "HALT"
    show Nop = "NOP"
    show Ret = "RET"
    show (PutChar r) = "PUTCHAR " <> show (Register r)
    show (PutNum r) = "PUTNUM " <> show (Register r)
    show (GetChar r) = "GETCHAR " <> show (Register r)

instance Show ExtendedInstr where
    show (Native i) = show i
    show (Call t n argx rx) = 
        "CALL " <> show n <> " (" <> 
            intercalate ", " (show <$> argx)
            <> ") -> " <> show rx <> " # " <> show t
    show (Return reg) = "RET " <> show (Register reg)
    show (Output reg) = "PRINT " <> show (Register reg)
    show (Immediate t i r) = showBinInstr "MOV" t (Imm i) (Register r)
