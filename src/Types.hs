{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Control.Arrow
import Control.Category (Category (..))
import Data.Function (on)
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Profunctor
import GHC.Generics
import Prelude hiding ((.), id)


------------------------------------------------------------------------------
-- | Super high tech database type.
type DB = Map CardId ()


------------------------------------------------------------------------------
-- | A card existentialized over its note.
data Card = forall n. IsNote n => Card
  { cardNote     :: n Identity
  , cardFrontIxF :: IxF [String] (n Identity) String
  , cardBackIxF  :: IxF [String] (n Identity) String
  }

instance Eq Card where
  (==) = (==) `on` cardId

instance Ord Card where
  compare = compare `on` cardId


------------------------------------------------------------------------------
-- | A unique ID for a 'Card', generated automatically via the coeffect
-- dependencies of its fields.
newtype CardId  = CardId {unCardId :: String }
  deriving (Eq, Ord)

instance Show CardId where
  show = show . unCardId

instance Read CardId where
  readsPrec = fmap (fmap $ first CardId) . readsPrec


------------------------------------------------------------------------------
-- | Get the front side of a 'Card'.
cardFront :: Card -> String
cardFront Card{..} = runIxF cardNote cardFrontIxF


------------------------------------------------------------------------------
-- | Get the back side of a 'Card'.
cardBack :: Card -> String
cardBack Card{..} = runIxF cardNote cardBackIxF


------------------------------------------------------------------------------
-- | Construct a unique 'CardId' for a 'Card'.
cardId :: Card -> CardId
cardId Card{..} = CardId $ mconcat
  [ noteId cardNote
  , ":"
  , intercalate "+" $ getTags cardFrontIxF
  , ">"
  , intercalate "+" $ getTags cardBackIxF
  ]


------------------------------------------------------------------------------
-- | A function with an associated tag value.
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


------------------------------------------------------------------------------
-- | Used to provide the primary key of a note.
class Generic (n Identity) => IsNote n where
  noteId :: n Identity -> String


------------------------------------------------------------------------------
-- | Evaluate an 'IxF' as a function.
runIxF :: a -> IxF z a c -> c
runIxF = flip runIxF'


------------------------------------------------------------------------------
-- | Tag a function with a value, getting an 'IxF'.
tagged :: z -> (a -> b) -> IxF z a b
tagged = flip IxF

