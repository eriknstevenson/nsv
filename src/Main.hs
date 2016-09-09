{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson.Lens
import Lucid
import Network.HTTP.Types (status200, status404)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Network.Wreq as Wreq
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do

  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $ do
    status status200
    html . renderText $ defaultLayout generateIndex

  notFound $ do
    status status404
    html . renderText $ defaultLayout show404

defaultLayout :: Html () -> Html ()
defaultLayout content = do
  doctype_
  termWith "html" [lang_ "en"] $ do
    head_ $ do

      title_ "nsv"

      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
      meta_ [name_ "description", content_ "nsv"]
      meta_ [name_ "author", content_ "Erik Stevenson"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

      link_ [href_ "//fonts.googleapis.com/css?family=Raleway:400,300,600", rel_ "stylesheet", type_ "text/css"]
      link_ [href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.1.2/css/bulma.min.css", rel_ "stylesheet", type_ "text/css"]
      link_ [href_ "/css/default.css", rel_ "stylesheet", type_ "text/css"]

    body_ [class_ "layout-default", style_ "zoom: 1;"] $ do
      pageHeader
      content
      footer

generateIndex :: Html ()
generateIndex = do
  section_ [class_ "section is-medium"] $ do
    nav_ [class_ "level"] $ do
      div_ [class_ "level-item has-text-centered"] $ do
        p_ [class_ "heading"] "Tweets"
        p_ [class_ "title"] "3,456"
      div_ [class_ "level-item has-text-centered"] $ do
        p_ [class_ "heading"] "Following"
        p_ [class_ "title"] "123"
      div_ [class_ "level-item has-text-centered"] $ do
        p_ [class_ "heading"] "Followers"
        p_ [class_ "title"] "456K"
      div_ [class_ "level-item has-text-centered"] $ do
        p_ [class_ "heading"] "Likes"
        p_ [class_ "title"] "789"

pageHeader :: Html ()
pageHeader = return ()

footer :: Html ()
footer = footer_ $ div_ $ do
  hr_ []
  small_ "(c) 2016 Erik Stevenson"

show404 :: Html ()
show404 = do
  img_ [src_ "/img/not_found.jpg"]
  h1_ "page not found"
  p_ [class_ "tagline"] "the page you requested does not exist"
