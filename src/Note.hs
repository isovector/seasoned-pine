{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}

------------------------------------------------------------------------------
-- | Magic (aka shitty) implementation of coeffects over standard Haskell arrow
-- types.
module Note where

import qualified Control.Category as C
import           Data.Functor.Identity
import           Data.Monoid ((<>))
import           GHC.Generics
import           Prelude hiding ((.), id)


class LiftJuice s where
  juice :: s a b -> IxF [String] a b

instance LiftJuice (IxF [String]) where
  juice = C.id
  {-# INLINE juice #-}

instance LiftJuice (->) where
  juice = tagged mempty
  {-# INLINE juice #-}

(.) :: (LiftJuice s, LiftJuice t) => t b c -> s a b -> IxF [String] a c
b . a = juice b C.. juice a
{-# INLINE (.) #-}

data IxF z a b = IxF
  { runIxF' :: a -> b
  , ixfC :: z
  }

instance Functor (IxF z a) where
  fmap f (IxF f' z) = IxF (f C.. f') z

instance Monoid z => Applicative (IxF z a) where
  pure a = IxF (pure a) mempty
  IxF f z <*> IxF f' z' = IxF (f <*> f') (z <> z')

instance Monoid z => C.Category (IxF z) where
  id = IxF C.id mempty
  b . a = IxF (runIxF' b C.. runIxF' a)
              (ixfC b <> ixfC a)

runIxF :: a -> IxF z a c -> c
runIxF = flip runIxF'

tagged :: z -> (a -> b) -> IxF z a b
tagged = flip IxF

getTags :: LiftJuice s => s a b -> [String]
getTags = ixfC C.. juice


class Generic (n Identity) => IsNote n where
  noteId :: n Identity -> String

