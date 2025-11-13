{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR.Tac where

import Data.List (intercalate)
import Text.Printf
import IR.CompilerState
import Control.Monad.State
import Control.Monad.Fix
import Control.Monad.Writer (WriterT, Writer, MonadWriter)
import Analysis.Typecheck.Type (Type)

newtype TAC = TAC [(Label, Instr)]
    deriving (Eq, Show)

data Instr
    -- Return type if any; First args, then target
    = Add Type Reg Reg Reg
    | Sub Type Reg Reg Reg
    | And Type Reg Reg Reg
    | Mul Type Reg Reg Reg
    | Xor Type Reg Reg Reg
    | Or Type Reg Reg Reg
    | Not Type Reg Reg
    | Div Type Reg Reg Reg
    | Mov Type Reg Reg
    | Deref Type Reg Reg
    | Ref Type Reg Reg
    | Imm Type Int Reg
    | Jmp Label
    | Jz Reg Label
    | Call Type Label [Reg] Reg
    | Push Reg
    | Pop Type Reg
    | Ret Reg
    | Halt
    | Nop
    | Output Reg
    | Input Type Reg
    deriving (Show, Eq)

instance Semigroup TAC where
    TAC a <> TAC b = TAC (a <> b)

instance Monoid TAC where
    mempty = TAC []

newtype Emitter a = Emitter (WriterT TAC (State CState) a)
    deriving (Functor, Applicative, Monad, MonadState CState, MonadFix, MonadWriter TAC)
