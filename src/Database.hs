{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import qualified Data.Text.Lazy as T
import           Data.Typeable
import           System.Random

data PostState = PostState {title :: T.Text, author :: T.Text}
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''PostState)

setPost :: T.Text -> T.Text -> Update PostState ()
setPost t a = put (PostState t a)

getTitle :: Query PostState T.Text
getTitle = do
  (PostState t _) <- ask
  return t

$(makeAcidic ''PostState ['setPost, 'getTitle])

runDB q = do
  db <- lift ask
  liftIO (query db q)

setDB t = do
  db <- lift ask
  liftIO (update db $ SetPost t "never seen")