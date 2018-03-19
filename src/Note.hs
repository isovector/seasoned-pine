{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

------------------------------------------------------------------------------
-- | Magic (aka shitty) implementation of coeffects over standard Haskell arrow
-- types.
module Note where

import Data.Proxy
import GHC.TypeLits
import Data.Functor.Identity
import qualified Control.Category as C
import Prelude hiding ((.), id)
import Data.Monoid ((<>))
import GHC.Generics

class LiftJuice z s where
  juice :: s a b -> IxF z a b

instance LiftJuice z (IxF z) where
  juice = C.id
  {-# INLINE juice #-}

instance Monoid z => LiftJuice z (->) where
  juice = tagged mempty
  {-# INLINE juice #-}

(.) :: (Monoid z, LiftJuice z s, LiftJuice z t) => t b c -> s a b -> IxF z a c
b . a = juice b C.. juice a
{-# INLINE (.) #-}

data IxF z a b = IxF
  { runIxF :: a -> b
  , ixfC :: z
  }

instance Monoid z => C.Category (IxF z) where
  id = IxF C.id mempty
  b . a = IxF (runIxF b C.. runIxF a)
              (ixfC b <> ixfC a)

tagged :: z -> (a -> b) -> IxF z a b
tagged = flip IxF

data Note = Note

name :: IxF String a String
name = tagged "name" $ const "hello"

getTags :: LiftJuice z s => s a b -> z
getTags = ixfC C.. juice


type family Sel f a where
  Sel Identity a = a
  Sel f a = f a

data X f = X { getFoo :: Sel f String }
  deriving Generic

class GetCoeffects s where
  getCoeffects :: s (IxF [String] (s Identity))

class GGetCoeffects (f :: * -> *) g where
  ggetCoeffects :: g p

instance (GGetCoeffects f f', GGetCoeffects g g') => GGetCoeffects (f :*: g) (f' :*: g') where
  ggetCoeffects =  ggetCoeffects @f :*: ggetCoeffects @g

-- instance ( GGetCoeffects f f'
--          -- , f' ~ IxF [String] z
--          , KnownSymbol name
--          )
--     => GGetCoeffects (M1 S ('MetaSel ('Just name) a b c) f)
--                      (M1 S ('MetaSel ('Just name) a b c) (Rep (IxF f'))) where
--   ggetCoeffects = M1 . from . tagged [symbolVal $ Proxy @name] . to $ ggetCoeffects @f


