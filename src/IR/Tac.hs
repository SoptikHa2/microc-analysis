{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR.Tac where

import Data.List (intercalate)
import Text.Printf
import IR.CompilerState
import Control.Monad.State
import Control.Monad.Fix
import Control.Monad.Writer (WriterT, Writer, MonadWriter)

newtype IR = IR [(Label, Instr)]
    deriving (Eq, Show)

data Instr
    -- First args, then target
    = Add Reg Reg Reg
    | Sub Reg Reg Reg
    | And Reg Reg Reg
    | Mul Reg Reg Reg
    | Xor Reg Reg Reg
    | Or Reg Reg Reg
    | Not Reg Reg
    | Div Reg Reg Reg
    | Mov Reg Reg Reg
    | Deref Reg Reg
    | Ref Reg Reg
    | Imm Int Reg
    | Jmp Label
    | Jz Reg Label
    | Call Label [Reg] Reg
    | Push Reg
    | Pop Reg
    | Ret Reg
    | Halt
    | Nop
    deriving (Show, Eq)

instance Semigroup IR where
    IR a <> IR b = IR (a <> b)

instance Monoid IR where
    mempty = IR []

newtype TAC a = TAC (WriterT IR (State CState) a)
    deriving (Functor, Applicative, Monad, MonadState CState, MonadFix, MonadWriter IR)
