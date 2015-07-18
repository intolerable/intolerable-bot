module Control.Applicative.Trans.Either
  ( EitherA(..)
  , failWith
  , justOr ) where

import Control.Applicative

newtype EitherA e f a = EitherA { runEitherA :: f (Either e a) }

deriving instance Show (f (Either e a)) => Show (EitherA e f a)
deriving instance Read (f (Either e a)) => Read (EitherA e f a)
deriving instance Eq (f (Either e a)) => Eq (EitherA e f a)
deriving instance Ord (f (Either e a)) => Ord (EitherA e f a)

instance Functor f => Functor (EitherA e f) where
  fmap f (EitherA x) = EitherA (fmap (fmap f) x)

instance (Monoid e, Applicative f) => Applicative (EitherA e f) where
  pure = EitherA . pure . Right
  EitherA f <*> EitherA x = EitherA (combine <$> f <*> x)

instance (Monoid e, Alternative f) => Alternative (EitherA e f) where
  empty = EitherA empty
  EitherA x <|> EitherA y = EitherA (alternative <$> x <*> y)

instance (Monoid e, Alternative f) => Monoid (EitherA e f a) where
  mempty = empty
  mappend = (<|>)

combine :: Monoid a => Either a (b -> c) -> Either a b -> Either a c
combine (Left x) (Left y) = Left (x `mappend` y)
combine (Left x) _ = Left x
combine _ (Left y) = Left y
combine (Right g) (Right y) = Right (g y)

alternative :: Monoid a => Either a b -> Either a b -> Either a b
alternative (Right x) _ = Right x
alternative (Left _) (Right x) = Right x
alternative (Left x) (Left y) = Left (x `mappend` y)

failWith :: Applicative f => e -> EitherA e f a
failWith = EitherA . pure . Left

justOr :: (Applicative f, Monoid e) => e -> Maybe a -> EitherA e f a
justOr m = maybe (failWith m) pure
