{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Acid
import qualified Data.Text as T
import           Lucid
import           Network.HTTP.Types
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Random
import           Web.Scotty

main :: IO ()
main = do

  forkIO manageDatabase

  scotty 3000 $ do

    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
      status status200
      html . renderText $ defaultLayout index

    notFound $ do
      status status404
      html . renderText $ defaultLayout show404

manageDatabase :: IO ()
manageDatabase = forever $ do
  putStrLn "updating database."
  threadDelay 10000000

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
