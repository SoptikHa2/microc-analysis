module Lattice (Lattice(..)) where

class Eq a => Lattice a where
    top :: a
    bottom :: a
    (<&>) :: a -> a -> a
    (<|>) :: a -> a -> a

