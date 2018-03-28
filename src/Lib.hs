{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Lib where

import           Control.Arrow (first)
import           Control.Monad (void)
import           Control.Newtype
import           Data.Foldable (for_)
import           Data.Function (fix, on)
import           Data.Functor.Identity
import           Data.List (intercalate)
import           Data.Maybe (listToMaybe, fromJust)
import           Data.Monoid ((<>), First (..))
import qualified Note as N
import           Note hiding ((.))
import           System.IO (hSetBuffering, hSetEcho, BufferMode (NoBuffering), stdin)


data Difficulty
  = Again
  | Learning
  | Hard
  | Medium
  | Easy
  deriving (Eq, Ord, Enum, Bounded, Show)

data Card = forall n. IsNote n => Card
  { cardNote     :: n Identity
  , cardFrontIxF :: IxF [String] (n Identity) String
  , cardBackIxF  :: IxF [String] (n Identity) String
  }

newtype CardId  = CardId {unCardId :: String }
  deriving (Eq, Ord)

instance Show CardId where
  show = show . unCardId

instance Read CardId where
  readsPrec = fmap (fmap $ first CardId) . readsPrec

instance Eq Card where
  (==) = (==) `on` cardId

instance Ord Card where
  compare = compare `on` cardId

cardFront :: Card -> String
cardFront Card{..} = runIxF cardNote cardFrontIxF

cardBack :: Card -> String
cardBack Card{..} = runIxF cardNote cardBackIxF

cardId :: Card -> CardId
cardId Card{..} = CardId $ mconcat
  [ noteId cardNote
  , ":"
  , intercalate "+" $ getTags cardFrontIxF
  , ">"
  , intercalate "+" $ getTags cardBackIxF
  ]

makeCard
    :: ( IsNote n
       )
    => IxF [String] (n Identity) String
    -> IxF [String] (n Identity) String
    -> n Identity
    -> Card
makeCard front back n = Card n front back


runCard :: Card -> IO Difficulty
runCard c = do
  clearScreen
  putStrLn $ cardFront c
  dumpLines 6
  hSetEcho stdin False
  void getChar

  clearScreen
  putStrLn $ cardBack c
  dumpLines 1
  difficultySelector


difficultySelector :: IO Difficulty
difficultySelector = do
  for_ (zip [1..] [Again .. Easy]) $ \(n,d) ->
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

