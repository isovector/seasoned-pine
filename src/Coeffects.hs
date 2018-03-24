{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Coeffects where

import Control.Category (id, (.))
import Data.Functor.Identity
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Note hiding ((.))
import Prelude hiding (id, (.))

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
  ggetCoeffects sel = M1 $ ggetCoeffects @s @g @o $ (\(M1 x) -> x) . sel

instance {-# OVERLAPPING #-} KnownSymbol name
    => GGetCoeffects s (M1 S ('MetaSel ('Just name) _a _b _c) (K1 R z))
                       (M1 S ('MetaSel ('Just name) _a _b _c)
                         (K1 R (IxF [String] (s Identity) z))) where
  ggetCoeffects sel = M1 $ K1 $ tagged [symbolVal $ Proxy @name] $ \si ->
    case sel si of M1 (K1 x) -> x

instance (GGetCoeffects s f o, GGetCoeffects s f' o')
    => GGetCoeffects s (f :*: f') (o :*: o') where
  ggetCoeffects sel = ggetCoeffects @s @f  @o  ((\(a :*: _) -> a) . sel)
                  :*: ggetCoeffects @s @f' @o' ((\(_ :*: b) -> b) . sel)

getCoeffects
    :: forall s si ixf
     . ( si  ~ s Identity
       , ixf ~ s (IxF [String] (s Identity))
       , Generic si
       , Generic ixf
       , GGetCoeffects s (Rep si) (Rep ixf)
       ) => s (IxF [String] (s Identity))
getCoeffects = to $ ggetCoeffects @s @(Rep (s Identity)) from

