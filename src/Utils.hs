module Utils ((<+>)) where


infixl 4 <+>
(<+>) :: Applicative f => f (a -> b) -> a -> f b
(<+>) l r = l <*> pure r