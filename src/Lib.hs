{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Lib
    ( someFunc
    ) where

import Data.Function (fix)
import Control.Newtype
import System.IO (hSetBuffering, hSetEcho, BufferMode (NoBuffering), stdin)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.Monoid ((<>), First (..))
import Data.Maybe (listToMaybe, fromJust)


data Id = Id
  deriving (Eq, Ord)

data Difficulty
  = Again
  | Learning
  | Hard
  | Medium
  | Easy
  deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card
  { cardId    :: Id
  , cardFront :: String
  , cardBack  :: String
  }

runCard :: Card -> IO Difficulty
runCard c = do
  clearScreen
  putStrLn $ cardFront c
  dumpLines 6
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
someFunc = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  print =<< (runCard $ Card Id "hello" "goodbye")

