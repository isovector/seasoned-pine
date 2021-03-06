{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module MyNotes where

import           Coeffects
import           Control.Applicative (liftA2)
import           Control.Category
import           Data.Functor.Identity
import           Data.Monoid ((<>))
import           GHC.Exts
import           Lib
import qualified MyTypes as T
import           Prelude hiding (id, (.))
import           Types


T.Verb {..} = getCoeffects

instance Monoid z => IsString (IxF z a String) where
  fromString = pure


verbs :: [T.VerbNote Identity]
verbs =
  [ T.Verb "būti"      "to be"              "COMPLICATED" "yra"       "UNKNOWN"
  , T.Verb "daryti"    "to make/do"         "ką"          "daro"      "UNKNOWN"
  , T.Verb "dirbti"    "to work"            "ką*"         "dirba"     "UNKNOWN"
  , T.Verb "eiti"      "to go (on foot)"    "į kur"       "eita"      "UNKNOWN"
  , T.Verb "gerti"     "to drink"           "ką"          "geria"     "UNKNOWN"
  , T.Verb "gyventi"   "to live"            "kur"         "gyvena"    "UNKNOWN"
  , T.Verb "išsinešti" "to take away"       "ką???"       "išsineša"  "išsinešė"
  , T.Verb "kalbėti"   "to speak"           "ką"          "kalba"     "UNKNOWN"
  , T.Verb "mokėti"    "to pay for/to know" "ką"          "moka"      "mokėjo"
  , T.Verb "mokytis"   "to learn"           "ko"          "mokosi"    "UNKNOWN"
  , T.Verb "mėgti"     "to like (2)"        "ką"          "mėgsta"    "UNKNOWN"
  , T.Verb "norėti"    "to want"            "ko"          "nori"      "UNKNOWN"
  , T.Verb "pirkti"    "to buy"             "ką"          "perka"     "UNKNOWN"
  , T.Verb "suprasti"  "to understand"      "ką"          "supranta"  "UNKNOWN"
  , T.Verb "susitikti" "to meet up"         "ką"          "susitinka" "susitiko"
  , T.Verb "turėti"    "to have"            "ką"          "turi"      "UNKNOWN"
  , T.Verb "valgyti"   "to eat"             "ką"          "valgo"     "UNKNOWN"
  , T.Verb "važiuoti"  "to go (by vehicle)" "į kur"       "važiuoja"  "UNKNOWN"
  , T.Verb "žaisti"    "to play"            "ką"          "žaidžia"   "UNKNOWN"
  , T.Verb "žinoti"    "to know"            "ką"          "žino"      "UNKNOWN"
  , T.Verb "žiūrėti"   "to watch/look"      "kur"         "žiūri"     "UNKNOWN"
  ]

verbCards :: [Card]
verbCards = do
  note     <- verbs
  template <-
    [ makeCard
        (mconcat ["What is '", vLt, "' (", vCase, ") in English?"])
        vEn
    , makeCard
        (mconcat ["What is '", vEn, "' in Lithuanian?"])
        vLt
    , makeCard
        (mconcat ["Jis/Ji [", vLt, "]?"])
        vJisJi
    , makeCard
        (mconcat ["Case for ", vLt, "?"])
        vCase
    , makeCard
        (mconcat ["What is the infinitive of this? [", vJisJi, "]"])
        vLt
    ]

  pure $ template note

