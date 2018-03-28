{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Study where

import Data.Bool (bool)
import Control.Lens
import Data.Time


data Difficulty
  = Again
  | Learning
  | Hard
  | Medium
  | Easy
  deriving (Eq, Ord, Enum, Bounded, Show)

data Phase = BeingLearned | Learned | Relapsed
  deriving (Eq, Ord, Show, Enum, Bounded, Read)

instance Read NominalDiffTime where
  readsPrec _ s =
    let (a, b) = break (== 's') s
     in [(fromIntegral (read a :: Int), drop 1 b)]


data Study = Study
  { _sLastScheduled  :: UTCTime
  , _sReviewDuration :: NominalDiffTime
  , _sPhase          :: Phase
  } deriving (Eq, Ord, Show, Read)
makeLenses ''Study


relapseTime :: NominalDiffTime
relapseTime = 120


learningTime :: NominalDiffTime
learningTime = 600


firstEasyTime :: NominalDiffTime
firstEasyTime = 24 * 3600 * 4


getNextDuration
    :: Difficulty
    -> Phase
    -> NominalDiffTime
    -> (NominalDiffTime, Phase)
getNextDuration Again x _ =
  (relapseTime, bool Relapsed BeingLearned $ x == BeingLearned)
getNextDuration Learning _ _        = (learningTime,  BeingLearned)
getNextDuration Easy BeingLearned _ = (firstEasyTime, Learned)
getNextDuration Easy _ t            = (t * 2,         Learned)
getNextDuration Medium _ t          = (t,             Learned)
getNextDuration Hard _ t            = (t / 2,         Learned)


updateStudy :: UTCTime -> Difficulty -> Study -> Study
updateStudy now d s =
  let (time', phase') = getNextDuration d (_sPhase s) (_sReviewDuration s)
   in s & sLastScheduled  .~ now
        & sReviewDuration .~ time'
        & sPhase          .~ phase'

