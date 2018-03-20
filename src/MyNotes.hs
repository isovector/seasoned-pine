{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module MyNotes where

import qualified Types as T
import Note
import Lib

T.Verb {..} = getCoeffects

x :: Card
x = makeCard vLt vEn $ T.Verb "kalbėti" "to speak" "ką" "kalba" "kalbėjo"


