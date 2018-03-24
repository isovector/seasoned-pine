{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Persistence where

import Data.Map (Map)
import qualified Data.Map as M
import Lib
import Control.Exception


type DB = Map CardId ()

initDB :: IO DB
initDB = do
  let db = M.fromList []
  saveDB db
  pure db

loadDB :: IO DB
loadDB = fmap read $ readFile "deck.db"

loadOrInitDB :: IO DB
loadOrInitDB = do
  try loadDB >>= \case
    Left (e :: SomeException) -> initDB
    Right db                  -> pure db

saveDB :: DB -> IO ()
saveDB = writeFile "deck.db" . show

withDB :: (DB -> IO DB) -> IO ()
withDB io = loadDB >>= io >>= saveDB

