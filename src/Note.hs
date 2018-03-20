{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

------------------------------------------------------------------------------
-- | Magic (aka shitty) implementation of coeffects over standard Haskell arrow
-- types.
module Note where

import qualified Control.Category as C
import           Data.Functor.Identity
import           Data.Monoid ((<>))
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Prelude hiding ((.), id)

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
  { runIxF' :: a -> b
  , ixfC :: z
  } deriving (Generic)

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

getTags :: LiftJuice z s => s a b -> z
getTags = ixfC C.. juice


type family Sel f a where
  Sel Identity a = a
  Sel f a = f a

class GGetCoeffects s g o where
  ggetCoeffects :: (s Identity -> g p) -> o p

instance GGetCoeffects s V1 V1 where
  ggetCoeffects = undefined

instance GGetCoeffects s U1 U1 where
  ggetCoeffects = const U1

instance GGetCoeffects s g o => GGetCoeffects s (M1 i c g) (M1 i c' o) where
  ggetCoeffects sel = M1 $ ggetCoeffects @s @g @o $ (\(M1 x) -> x) C.. sel

instance {-# OVERLAPPING #-} KnownSymbol name
    => GGetCoeffects s (M1 S ('MetaSel ('Just name) _a _b _c) (K1 R z))
                       (M1 S ('MetaSel ('Just name) _a _b _c)
                         (K1 R (IxF [String] (s Identity) z))) where
  ggetCoeffects sel = M1 $ K1 $ tagged [symbolVal $ Proxy @name] $ \si ->
    case sel si of M1 (K1 x) -> x

instance (GGetCoeffects s f o, GGetCoeffects s f' o')
    => GGetCoeffects s (f :*: f') (o :*: o') where
  ggetCoeffects sel = ggetCoeffects @s @f  @o  ((\(a :*: _) -> a) C.. sel)
                  :*: ggetCoeffects @s @f' @o' ((\(_ :*: b) -> b) C.. sel)

getCoeffects
    :: forall s si ixf
     . ( si  ~ s Identity
       , ixf ~ s (IxF [String] (s Identity))
       , Generic si
       , Generic ixf
       , GGetCoeffects s (Rep si) (Rep ixf)
       ) => s (IxF [String] (s Identity))
getCoeffects = to $ ggetCoeffects @s @(Rep (s Identity)) from

