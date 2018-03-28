{-# LANGUAGE RecordWildCards   #-}

------------------------------------------------------------------------------
-- | Magic (aka shitty) implementation of coeffects over standard Haskell arrow
-- types.
module Note where

import           Control.Arrow
import           Control.Category (Category (..))
import           Data.Functor.Identity
import           Data.Monoid ((<>))
import           Data.Profunctor
import           GHC.Generics
import           Prelude hiding ((.), id)


data IxF z a b = IxF
  { runIxF' :: a -> b
  , getTags :: z
  }

instance Functor (IxF z a) where
  fmap f (IxF f' z) = IxF (f . f') z

instance (Monoid z, Monoid b) => Monoid (IxF z a b) where
  mempty = tagged mempty $ const mempty
  mappend (IxF f z) (IxF f' z') = IxF (f <> f') (z <> z')

instance Monoid z => Applicative (IxF z a) where
  pure a = IxF (pure a) mempty
  IxF f z <*> IxF f' z' = IxF (f <*> f') (z <> z')

instance Monoid z => Category (IxF z) where
  id = IxF id mempty
  b . a = IxF (runIxF' b . runIxF' a)
              (getTags b <> getTags a)

instance Monoid z => Arrow (IxF z) where
  arr            = tagged mempty
  first IxF {..} = IxF (first runIxF') getTags

instance Monoid z => ArrowChoice (IxF z) where
  left IxF {..} = IxF (left runIxF') getTags


runIxF :: a -> IxF z a c -> c
runIxF = flip runIxF'

tagged :: z -> (a -> b) -> IxF z a b
tagged = flip IxF


class Generic (n Identity) => IsNote n where
  noteId :: n Identity -> String

