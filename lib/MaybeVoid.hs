module MaybeVoid where

-- * Type

data MaybeVoid a = NonVoid a | Void
  deriving (Show)

instance Eq a => Eq (MaybeVoid a) where
  NonVoid lhs == NonVoid rhs = lhs == rhs
  Void == Void = True
  _ == _ = False

instance Functor MaybeVoid where
  fmap f (NonVoid x) = NonVoid $ f x
  fmap _ Void = Void

instance Applicative MaybeVoid where
  pure = NonVoid

  NonVoid a <*> NonVoid x = NonVoid $ a x
  Void <*> _ = Void
  _ <*> Void = Void

instance Monad MaybeVoid where
  NonVoid x >>= mf = mf x
  Void >>= _ = Void

instance Foldable MaybeVoid where
  foldMap f (NonVoid x) = f x
  foldMap _ Void = mempty

instance Traversable MaybeVoid where
  traverse f (NonVoid x) = NonVoid <$> f x
  traverse _ Void = pure Void

-- * Utils

maybeVoidToMaybe :: MaybeVoid a -> Maybe a
maybeVoidToMaybe (NonVoid x) = Just x
maybeVoidToMaybe Void = Nothing

maybeToMaybeVoid :: Maybe a -> MaybeVoid a
maybeToMaybeVoid (Just x) = NonVoid x
maybeToMaybeVoid Nothing = Void

maybeVoid :: b -> (a -> b) -> MaybeVoid a -> b
maybeVoid _ nv (NonVoid x) = nv x
maybeVoid v _ Void = v
