{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where

import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import qualified Data.Text.Lazy as T
import           Data.Typeable
import           System.Random

type Database = AcidState Posts

data Entry = Entry {title :: T.Text, author :: T.Text}
  deriving (Show, Typeable)

data Posts = Posts [Entry]
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Entry)
$(deriveSafeCopy 0 'base ''Posts)

addEntry :: Entry -> Update Posts ()
addEntry e = do
  Posts entries <- get
  put (Posts $ e:entries)

clearEntries :: Update Posts ()
clearEntries = put $ Posts []

getEntries :: Query Posts [Entry]
getEntries = do
  Posts t <- ask
  return t

$(makeAcidic ''Posts ['addEntry, 'clearEntries, 'getEntries])

initDB :: IO (AcidState Posts)
initDB = openLocalState (Posts [])

queryState :: ReaderT (AcidState (EventState GetEntries)) IO (EventResult GetEntries)
queryState = do
 db <- ask
 liftIO (query db GetEntries)

updateState :: T.Text
            -> T.Text
            -> ReaderT (AcidState Posts) IO ()
updateState t a = do
  db <- ask
  liftIO (update db $ AddEntry $ Entry t a)

manageDatabase :: ReaderT (AcidState Posts) IO ()
manageDatabase = forever $ do
  res <- queryState
  liftIO . print $ res
  liftIO . threadDelay $ 10000000

readRandom :: ReaderT (AcidState Posts) IO (Maybe Entry)
readRandom = do
  entries <- queryState
  case entries of
    [] -> lift $ return Nothing
    xs -> liftIO $ (Just . (xs !!)) <$> randomRIO (0, length xs)
