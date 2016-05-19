{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import qualified Data.Text.Lazy as T
import           Data.Typeable
import           Lucid
import           Network.HTTP.Types (status200, status404)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Random
import qualified Web.Scotty.Trans as S

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

database :: [PostState]
database = []

--runDB :: (IsAcidic st, QueryEvent event) => event -> ReaderT (AcidState st) IO (EventResult event)
runDB q = do
  db <- lift ask
  liftIO (query db q)

main :: IO ()
main = do

  db <- openLocalState (PostState "Hello World" "Erik")
  forkIO (runReaderT manageDatabase db)
  S.scottyT 3000 (`runReaderT` db) app

app :: S.ScottyT T.Text (ReaderT (AcidState PostState) IO) ()
app = do

  S.middleware logStdoutDev
  S.middleware $ staticPolicy (noDots >-> addBase "static")

  S.get "/" $ do
    S.status status200
    S.html . renderText $ defaultLayout index

  S.get "/db" $ do
    S.status status200
    q <- runDB GetTitle
    S.text . T.pack . show $ q

  S.notFound $ do
    S.status status404
    S.html . renderText $ defaultLayout show404

manageDatabase :: IsAcidic st => ReaderT (AcidState st) IO ()
manageDatabase = forever $ do
  liftIO . putStrLn $ "updating database."
  liftIO . threadDelay $ 10000000

defaultLayout :: Html () -> Html ()
defaultLayout body = do
  doctype_
  termWith "html" [lang_ "en"] $ do
    head_ $ do

      title_ "nsv"

      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
      meta_ [name_ "description",          content_ "nsv"]
      meta_ [name_ "author",               content_ "Erik Stevenson"]
      meta_ [name_ "viewport",             content_ "width=device-width, initial-scale=1"]

      link_ [href_ "//fonts.googleapis.com/css?family=Raleway:400,300,600", rel_ "stylesheet", type_ "text/css"]
      link_ [href_ "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css", rel_ "stylesheet", type_ "text/css"]
      link_ [href_ "https://cdnjs.cloudflare.com/ajax/libs/normalize/4.1.1/normalize.min.css", rel_ "stylesheet", type_ "text/css"]

      --link_ [href_ "/css/skeleton.css", rel_ "stylesheet", type_ "text/css"]
      --link_ [href_ "/css/normalize.css", rel_ "stylesheet", type_ "text/css"]

      link_ [href_ "/css/default.css", rel_ "stylesheet", type_ "text/css"]

    body_ $ with div_ [class_ "container"] $ do
      body
      footer

index :: Html ()
index = do
  h1_ "hello world"
  ul_ $ do
    li_ "list item 1"
    li_ "list item 2"

footer :: Html ()
footer = footer_ $ div_ $ do
  hr_ []
  small_ "(c) 2016 Erik Stevenson"

show404 :: Html ()
show404 = do
  img_ [src_ "/img/not_found.jpg"]
  h1_ "page not found"
  p_ [class_ "tagline"] "the page you requested does not exist"
