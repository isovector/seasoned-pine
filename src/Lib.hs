{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall            #-}

module Lib where

import           Control.Error.Util
import           Control.Lens (ala)
import           Control.Monad (void, guard)
import           Control.Monad.Trans (lift)
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>), Endo (..))
import           Data.Time (UTCTime(), getCurrentTime, addUTCTime)
import           Data.Traversable (for)
import           Persistence
import           Study
import           System.IO (hSetEcho, stdin)
import           Test.QuickCheck (generate, shuffle)
import           Types



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
  dumpLines 7
  hSetEcho stdin False
  void getChar

  clearScreen
  putStrLn $ cardFront c
  putStrLn $ "> " <> cardBack c
  dumpLines 1
  d   <- difficultySelector
  now <- getCurrentTime
  pure $ updateStudy now d s


getCardsToReview :: [Card] -> UTCTime -> DB -> [(CardId, Study)]
getCardsToReview cards now db = do
  c <- cards
  let cid = cardId c

  case M.lookup cid db of
    Just s  -> do
      guard $ now >= addUTCTime (_sReviewDuration s) (_sLastScheduled s)
      pure (cid, s)
    Nothing -> pure (cid, Study (addUTCTime (-1) now) 0 BeingLearned)


runDatabase :: [Card] -> DB -> IO DB
runDatabase rawCards db = do
  let cards = M.fromList $ fmap (\c -> (cardId c, c)) rawCards
  now     <- getCurrentTime
  studies <- generate . shuffle $ getCardsToReview rawCards now db
  f <- for studies $ \(cid, s) -> maybeT (pure id) pure $ do
    c  <- hoistMaybe $ M.lookup cid cards
    s' <- lift $ runCard c s
    pure $ M.insert cid s'
  pure $ ala Endo foldMap f db


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


doReview :: [Card] -> IO ()
doReview = withDB . runDatabase

