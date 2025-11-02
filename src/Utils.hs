module Utils ((<+>), (<||>)) where


infixl 4 <+>
(<+>) :: Applicative f => f (a -> b) -> a -> f b
(<+>) l r = l <*> pure r

infixl 4 <||>
(<||>) :: Either a b -> Either a b -> Either a b
(<||>) (Left _) b = b
(<||>) a@(Right _) _ = a
