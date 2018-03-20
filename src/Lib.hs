{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Lib where

import           Control.Monad (void)
import           Control.Newtype
import           Data.Foldable (for_)
import           Data.Function (fix)
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

data Card = Card
  { cardFront :: IxF [String] () String
  , cardBack  :: IxF [String] () String
  }

cardId :: Card -> String
cardId c = intercalate "->"
         [ intercalate "+" $ getTags $ cardFront c
         , intercalate "+" $ getTags $ cardBack c
         ]

makeCard :: LiftJuice [String] s => s n String -> s n String -> n -> Card
makeCard front back n =
  Card (juice @[String] front N.. const n)
       (juice @[String] back  N.. const n)

runCard :: Card -> IO Difficulty
runCard c = do
  clearScreen
  putStrLn $ runIxF () $ cardFront c
  dumpLines 6
  void getChar

  clearScreen
  putStrLn $ runIxF () $ cardBack c
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
  print =<< (runCard $ makeCard (const "hello") (const "goodbye") undefined)

