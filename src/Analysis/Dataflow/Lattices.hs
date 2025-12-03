module Analysis.Dataflow.Lattices (ConstLattice(..)) where

import Lattice (Lattice (..))

data ConstLattice
    = Top
    | Const Int
    | Bottom
    deriving (Eq)

instance Show ConstLattice where
    show Top = "⊤"
    show Bottom = "⊥"
    show (Const i) = show i

instance Lattice ConstLattice where
    top = Top
    bottom = Bottom
    
    a <&> b | a == b = a
    Top <&> x = x
    x <&> Top = x
    Bottom <&> _ = Bottom
    _ <&> Bottom = Bottom
    Const _ <&> Const _ = Bottom

    a <|> b | a == b = a
    Top <|> _ = Top
    _ <|> Top = Top
    Bottom <|> x = x
    x <|> Bottom = x
    Const _ <|> Const _ = Top
