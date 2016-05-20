{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Concurrent (forkIO)
import           Control.Monad.Reader
import qualified Data.Text.Lazy as T
import           Lucid
import           Network.HTTP.Types (status200, status404)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import qualified Web.Scotty.Trans as S

import Database

main :: IO ()
main = do

  db <- initDB
  _ <- forkIO (runReaderT manageDatabase db)
  S.scottyT 3000 (`runReaderT` db) app

app :: S.ScottyT T.Text Database ()
app = do

  S.middleware logStdoutDev
  S.middleware $ staticPolicy (noDots >-> addBase "static")

  S.get "/" $ do
    S.status status200
    idx <- lift index
    S.html . renderText $ defaultLayout idx

  S.get "/db" $ do
    S.status status200
    q <- lift readRandom
    S.text . T.pack . show $ q

  S.get "/db/:newtitle" $ do
    newTitle <- S.param "newtitle"
    old <- lift queryState
    lift $ updateState newTitle "new author"
    S.text $ mconcat
      [ "Previous db held:"
      , T.pack . show $ old
      , "\r\nupdated contents of db to:"
      , T.pack . show $ newTitle
      ]

  S.notFound $ do
    S.status status404
    S.html . renderText $ defaultLayout show404

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

index :: Database (Html ())
index = do
  db <- ask
  return $ do
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

