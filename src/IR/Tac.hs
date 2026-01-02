{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module IR.Tac where

import IR.CompilerState
import Control.Monad.State
import Control.Monad.Fix
import Control.Monad.Writer (WriterT, MonadWriter)
import Analysis.Typecheck.Type (Type)
import Data.List (intercalate)

type NativeTAC = TAC TinyCInstr
type ExtendedTAC = TAC ExtendedInstr

newtype TAC a = TAC [(Label, a)]
    deriving (Eq, Functor)

instance Show a => Show (TAC a) where
    show (TAC tx) = intercalate "\n" (show' <$> tx)
        where
            show' (label, row) = show label <> "\t\t" <> show row


data AnySource
    = Register Reg
    | Imm Int
    deriving (Eq)

data AnyTarget
    = Direct AnySource
    | Deref AnySource
    deriving (Eq)

dreg :: Reg -> AnyTarget
dreg = Direct . Register

data ExtendedInstr
    = Native TinyCInstr
    | Call Type Label [AnyTarget] AnyTarget
    | GetNthArg Type Int AnyTarget
    | Ret Reg
    | Output Reg
    | Immediate Type Int Reg
    deriving (Eq)

data TinyCInstr
    -- Return type if any; a [op]= b; first one is target
    = Add Type Reg AnyTarget
    | Sub Type Reg AnyTarget
    | And Type Reg AnyTarget
    | Mul Type Reg AnyTarget
    | Xor Type Reg AnyTarget
    | Or Type Reg AnyTarget
    | Not Type Reg
    | Div Type Reg AnyTarget
    | Mov Type AnyTarget AnyTarget
    | Lea Type Reg AnySource
    | Jmp Label
    | Jz Reg Label
    | Push AnySource
    | Pop Type Reg
    | Halt
    | Nop
    | PutChar Reg
    | PutNum Reg
    | GetChar Reg
    deriving (Eq)

instance Semigroup (TAC a) where
    TAC a <> TAC b = TAC (a <> b)

instance Monoid (TAC a) where
    mempty = TAC []

newtype Emitter a = Emitter (WriterT ExtendedTAC (State CState) a)
    deriving (Functor, Applicative, Monad, MonadState CState, MonadFix, MonadWriter ExtendedTAC)
