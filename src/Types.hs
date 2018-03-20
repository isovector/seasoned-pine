{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Note

data VerbNote f = Verb
  { vLt    :: Sel f String
  , vEn    :: Sel f String
  , vCase  :: Sel f String
  , vJisJi :: Sel f String
  , vPast  :: Sel f String
  } deriving (Generic)

