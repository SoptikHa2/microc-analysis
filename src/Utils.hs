module Utils ((<+>), (<||>), (!?), (<$$>)) where


infixl 4 <+>
(<+>) :: Applicative f => f (a -> b) -> a -> f b
(<+>) l r = l <*> pure r

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> c = (f <$>) <$> c

infixl 4 <||>
(<||>) :: Either a b -> Either a b -> Either a b
(<||>) (Left _) b = b
(<||>) a@(Right _) _ = a

-- Taken from newer base https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.List.html#%21%3F
infixl 4 !?
(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
