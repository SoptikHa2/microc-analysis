module IR.TacPrint where
import IR.Tac
import Data.List (intercalate)
import Analysis.Typecheck.Type

letterize :: Int -> String
letterize 0 = "A"
letterize n = nm' <> nmx
    where
        (nm:nmx) = letterize (n - 1)
        nm' = if nm /= 'Z' then succ nm : "" else "AZ"

instance Show AnySource where
    show (Register r) = letterize r <> "x"
    show (Imm i) = show i

instance Show AnyTarget where
    show (Direct s) = show s
    show (Deref s) = "*" <> show s

instance Show ExtendedInstr where
    show (Native i) = show i
    show (Call t n argx rx) = 
        "call " <> show n <> " (" <> 
            intercalate "," (show <$> argx)
            <> ") -> " <> show rx <> " :: " <> show t
    show (Ret reg) = "ret " <> show (Register reg)
    show (Output reg) = "print " <> show (Register reg)
    show (Immediate t i r) = showBinInstr "mov" t i r

showBinInstr :: (Show l, Show r) => String -> Type -> l -> r -> String
showBinInstr op t l r = op <> " " <> show l <> ", " <> show r <> " :: " <> show t

instance Show TinyCInstr where
    show (Add t l r) = showBinInstr "add" t (Register l) r
    show (Sub t l r) = showBinInstr "sub" t (Register l) r
    show (And t l r) = showBinInstr "and" t (Register l) r
    show (Mul t l r) = showBinInstr "mul" t (Register l) r
    show (Xor t l r) = showBinInstr "xor" t (Register l) r
    show (Or t l r)  = showBinInstr "or"  t (Register l) r
    show (Not t r) = "not " <> show (Register r) <> " :: " <> show t
    show (Div t l r) = showBinInstr "div" t (Register l) r
    show (Mov t l r) = showBinInstr "mov" t l r
    show (Lea t l r) = showBinInstr "lea" t (Register l) r
    show (Jmp label) = "jmp " <> show label
    show (Jz reg label) = "jz " <> show label <> ", " <> show reg
    show (Push val) = "push " <> show val
    show (Pop t r) = "pop " <> show (Register r) <> " :: " <> show t
    show (Halt) = "halt"
    show (Nop) = "nop"
    show (PutChar r) = "putChar" <> show (Register r)
    show (PutNum r) = "putNum" <> show (Register r)
    show (GetChar r) = "getChar" <> show (Register r)
