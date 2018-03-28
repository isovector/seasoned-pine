{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module MyTypes where

import Coeffects
import GHC.Generics
import Data.Functor.Identity
import Types


data VerbNote f = Verb
  { vLt    :: Sel f String
  , vEn    :: Sel f String
  , vCase  :: Sel f String
  , vJisJi :: Sel f String
  , vPast  :: Sel f String
  } deriving (Generic)

instance IsNote VerbNote where
  noteId = vLt

