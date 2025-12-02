module Lattice (Lattice(..), (<&&>), (<||>)) where
import qualified Data.Map as M

class Eq a => Lattice a where
    top :: a
    bottom :: a
    (<&>) :: a -> a -> a
    (<|>) :: a -> a -> a


(<&&>) :: Ord a => Lattice b => M.Map a b -> M.Map a b -> M.Map a b
lhs <&&> rhs = M.unionWith (<&>) lhs rhs

(<||>) :: Ord a => Lattice b => M.Map a b -> M.Map a b -> M.Map a b
lhs <||> rhs = M.unionWith (<|>) lhs rhs
