{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module MyNotes where

import           Control.Category
import           Data.Monoid ((<>))
import           GHC.Exts
import           Lib
import           Note
import           Prelude hiding (id, (.))
import qualified Types as T

T.Verb {..} = getCoeffects

instance IsString (IxF [String] a String) where
  fromString = pure

instance IsList (IxF [String] a String) where
  type Item (IxF [String] a String) = IxF [String] a String
  fromList = foldr (<<>>) (juice $ const mempty)
  toList   = pure

(<<>>) :: (Monoid z, Monoid b) => IxF z a b -> IxF z a b -> IxF z a b
a <<>> b = (<>) <$> a <*> b


x :: Card
x = makeCard ["What is '", vLt, "' (", vCase, ") in english?"]
             vEn $ T.Verb "kalbėti" "to speak" "ką" "kalba" "kalbėjo"


