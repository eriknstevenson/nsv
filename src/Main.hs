{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Network.HTTP.Types (ok200, notFound404)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Network.Wreq as Wreq
import System.Random
import Web.Scotty hiding (post)

main :: IO ()
main = scotty 3000 $ do

  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

  --TODO: fix this behavior
  get "/postsnotfound" $ do
    status ok200
    posts <- liftIO $ getPosts "pics" ["xxasdfsdf"]
    let count = length posts
    toDisplay <- liftIO $ (posts !!) <$> randomRIO (0, count)
    withDefaultLayout $ indexPage toDisplay

  --TODO: fix this behavior
  get "/subredditnotfound" $ do
    status ok200
    posts <- liftIO $ getPosts "sdf" ["sdf"]
    let count = length posts
    toDisplay <- liftIO $ (posts !!) <$> randomRIO (0, count)
    withDefaultLayout $ indexPage toDisplay

  get "/" $ do
    status ok200
    posts <- liftIO $ getPosts "loseit" ["sv", "nsv"]
    let count = length posts
    if count > 0
      then do
        post <- liftIO $ (posts !!) <$> randomRIO (0, count)
        withDefaultLayout $ indexPage post
      else
        withDefaultLayout shareYourNSV

  get "/about" $ do
    status ok200
    withDefaultLayout about

  notFound $ do
    status notFound404
    withDefaultLayout show404

getPosts :: Text -> [Text] -> IO [Post]
getPosts subreddit tags = do
  r <- Wreq.get $ "http://reddit.com/r/" <> T.unpack subreddit <> ".json"
  let results = r ^.. Wreq.responseBody . key "data" . key "children" . values . key "data" . search tags
  return . map makePost $ results

data Post = Post
  { getTitle :: Text
  , getAuthor :: Text
  , getURL :: Text
  } deriving (Show)

makePost :: Value -> Post
makePost e =
  Post (e ^. key "title" . _String)
       (e ^. key "author" . _String)
       ("http://reddit.com" <> e ^. key "permalink" . _String)

search :: [Text] -> Traversal' Value Value
search tags =
  let
    clean = T.toLower . T.strip
    cleanedTags = map clean tags
    getField f e = clean . fromMaybe "" $ e ^? key f . _String
    flairText = getField "link_flair_text"
    postTitle = getField "title"
    checkTags searchText = or $ do
      keyword <- cleanedTags
      return $ T.isInfixOf keyword searchText
  in
    case tags of
      [] -> filtered $ const True
      _ -> filtered (\e -> checkTags (flairText e) ||
                           checkTags (postTitle e))

data Page = Home | About | NotFound deriving (Show, Eq)

withDefaultLayout :: Html () -> ActionM ()
withDefaultLayout = html . renderText . defaultLayout

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
      link_ [href_ "/css/font-awesome.min.css", rel_ "stylesheet", type_ "text/css"]
      link_ [href_ "/css/default.css", rel_ "stylesheet", type_ "text/css"]

    body_ [class_ "layout-default", style_ "zoom: 1;"] content

indexPage :: Post -> Html ()
indexPage post =
  section_ [class_ "hero is-primary is-fullheight is-bold"] $ do
    pageHeader Home
    div_ [class_ "hero-body"] $
      div_ [class_ "container has-text-centered"] $ do
        a_ [href_ $ getURL post] $
          h3_ [class_ "title is-1"] $ toHtml . getTitle $ post
        a_ [href_ $ "http://reddit.com/u/" <> getAuthor post] $
          h4_ [class_ "subtitle is-4"] $ toHtml (" - " <> getAuthor post)
    div_ [class_ "hero-foot"] $
      div_ [class_ "container"] $
        div_ [class_ "nav-right"] $
          small_ "(c) 2016 Erik Stevenson" 

shareYourNSV :: Html ()
shareYourNSV = undefined

show404 :: Html ()
show404 = 
  section_ [class_ "hero is-large is-danger is-bold"] $ do
    pageHeader NotFound
    div_ [class_ "hero-body"] $
      div_ [class_ "container has-text-centered"] $ do
        h1_ [class_ "title"] "Page not found."
        h2_ [class_ "subtitle"] "The page you requested does not exist."

about :: Html ()
about = do
  section_ [class_ "hero is-medium is-primary"] $ do
    pageHeader About
    div_ [class_ "hero-body"] $
      div_ [class_ "container has-text-centered"] $
        h1_ [class_ "title"] "About"

  section_ [class_ "content"] $ do
    p_ "this is a test of the about page"
    p_ "this is paragraph #2"
    ul_ $ do
      li_ "item 1"
      li_ "item 2"

pageHeader :: Page -> Html ()
pageHeader activePage = 
  div_ [class_ "hero-head"] $
    header_ [class_ "nav"] $
      div_ [class_ "container"] $ do
        div_ [class_ "nav-left"] $
          a_ [class_ "nav-item"] $
            h4_ [class_ "title is-4"] "nsv"
        div_ [class_ "nav-right nav-menu"] $ do
          with (a_ [class_ "nav-item", href_ "/"] "Home") $ do
            guard (activePage == Home)
            return $ class_ " is-active"
          with (a_ [class_ "nav-item", href_ "/about"] "About") $ do
            guard (activePage == About)
            return $ class_ " is-active"
          span_ [class_ "nav-item"] $
            a_ [class_ "button is-inverted", href_ "https://github.com/narrative/nsv"] $ do
              span_ [class_ "icon"] $ i_ [class_ "fa fa-github"] ""
              span_ [] "Github"


