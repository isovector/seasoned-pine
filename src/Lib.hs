{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall    #-}

module Lib where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import Persistence
import Study
import System.IO (hSetEcho, stdin)
import Types



makeCard
    :: ( IsNote n
       )
    => IxF [String] (n Identity) String
    -> IxF [String] (n Identity) String
    -> n Identity
    -> Card
makeCard front back n = Card n front back


runCard :: Card -> Study -> IO Study
runCard c s = do
  clearScreen
  putStrLn $ cardFront c
  dumpLines 6
  hSetEcho stdin False
  void getChar

  clearScreen
  putStrLn $ cardBack c
  dumpLines 1
  d   <- difficultySelector
  now <- getCurrentTime
  pure $ updateStudy now d s


runDatabase :: DB -> IO DB
runDatabase db = do
  pure db


difficultySelector :: IO Difficulty
difficultySelector = do
  for_ (zip [1..] [Again .. Easy]) $ \(n :: Int, d) ->
    putStrLn $ show n <> ") " <> show d

  fix $ \f -> do
    hSetEcho stdin False
    str <- pure <$> getChar
    maybe f pure $ do
      x <- fmap fst . listToMaybe $ reads str
      safeToEnum $ subtract 1 x


safeToEnum :: forall t . (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i =
  if (i >= fromEnum (minBound :: t)) && (i <= fromEnum (maxBound :: t))
    then Just . toEnum $ i
    else Nothing


dumpLines :: Int -> IO ()
dumpLines = sequence_ . flip replicate (putStrLn "")


clearScreen :: IO ()
clearScreen = dumpLines 80


someFunc :: IO ()
someFunc = pure ()

