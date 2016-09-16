{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Lucid
import Network.HTTP.Types (ok200, notFound404)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Network.Wreq as Wreq
import System.Environment
import System.Random
import Text.Read hiding (get)
import Web.Scotty hiding (post)

main :: IO ()
main = do

  (storedPosts, postCount) <- atomically $
    (,) <$> newTVar V.empty <*> newTVar 0

  _ <- forkIO (updatePosts storedPosts postCount)

  port <- lookupEnv "PORT"

  scotty (fromMaybe (3000::Int) (port >>= readMaybe))$ do

    middleware logStdout
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
      status ok200
      (posts, count) <- liftIO . atomically $
        (,) <$> readTVar storedPosts <*> readTVar postCount
      if count > 0
        then do
          post <- liftIO $ (posts !) <$> randomRIO (0, count - 1)
          withDefaultLayout $ indexPage post
        else
          withDefaultLayout shareYourNSV

    get "/about" $ do
      status ok200
      withDefaultLayout about

    notFound $ do
      status notFound404
      withDefaultLayout show404

updatePosts :: TVar (Vector Post) -> TVar Int -> IO ()
updatePosts posts count =
  withDelay hour $ do
    results <- searchSubreddit "loseit" ["sv","nsv"] 25 `catchAll` (\_ -> return V.empty)
    atomically $ do
      writeTVar posts results
      writeTVar count $ V.length results

type Time = Int

hour :: Time
hour = 60 * minute

minute :: Time
minute = 60 * second

second :: Time
second = 1000000

withDelay :: Time -> IO a -> IO ()
withDelay delay action = forever $ action >> threadDelay delay

searchSubreddit :: Text -> [Text] -> Int -> IO (Vector Post)
searchSubreddit subreddit tags pageCount = do
  res <- getPages Wreq.defaults pageCount
  return . V.fromList . concat $ res
  where
    getPages opts count
      | count >= 1 = do
        (posts, nextReq) <- getPage opts
        rest <- getPages nextReq (count - 1)
        return $ posts : rest
      | otherwise = return []

    getPage opts = do
      r <- Wreq.getWith opts $ "http://reddit.com/r/" <> T.unpack subreddit <> ".json"
      let results = r ^.. Wreq.responseBody . key "data" . key "children" . values . key "data" . search tags
          nextReq = r ^. Wreq.responseBody . key "data" . key "after" . _String
      return (map makePost results, Wreq.defaults & Wreq.param "after" .~ [nextReq])

data Post = Post
  { getTitle :: Text
  , getAuthor :: Text
  , getURL :: Text
  , getDate :: Maybe UTCTime
  , getID :: Text
  }

instance Show Post where
  show (Post title _ _ _ _) = T.unpack title

makePost :: Value -> Post
makePost e =
  let
    date =
      posixSecondsToUTCTime . fromIntegral <$>
        e ^? key "created" . _Integer
    permalink =
      "http://reddit.com" <> e^.key "permalink" . _String
  in
    Post (e ^. key "title" . _String)
         (e ^. key "author" . _String)
         permalink
         date
         (e ^. key "id" . _String)

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

data Page = Home | About | Share | NotFound deriving (Show, Eq)

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

    body_ [class_ "layout-default", style_ "zoom: 1;"] $ content >> pageFooter

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

{-
 - Page that is displayed when there are no matching posts found.
 - Provides a link to submit a SV/NSV at /r/loseit.
-}
shareYourNSV :: Html ()
shareYourNSV =
  section_ [class_ "hero is-large"] $ do
    pageHeader About
    div_ [class_ "hero-body"] $
      div_ [class_ "container has-text-centered"] $ do
        h1_ [class_ "title"] "No posts found."
        a_ [href_ "https://www.reddit.com/r/loseit/submit?selftext=true"] $
          h2_ [class_ "subtitle"] "submit your own!"

show404 :: Html ()
show404 =
  section_ [class_ "hero is-large is-danger is-bold"] $ do
    pageHeader NotFound
    div_ [class_ "hero-body"] $
      div_ [class_ "container has-text-centered"] $ do
        h1_ [class_ "title"] "Page not found."
        h2_ [class_ "subtitle"] "The page you requested does not exist."

about :: Html ()
about =
  section_ [class_ "hero is-medium is-primary"] $ do
    pageHeader About
    div_ [class_ "hero-body"] $
      div_ [class_ "container has-text-centered"] $ do
        h1_ [class_ "title"] "NSV"
        a_ [href_ "https://www.reddit.com/r/loseit/submit?selftext=true"] $
          h3_ [class_ "subtitle"] "submit your own nsv!"


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

pageFooter :: Html ()
pageFooter =
  footer_ [class_ "footer"] $
    div_ [class_ "container"] $
      div_ [class_ "content has-text-centered"] $
        p_ "(c) 2016 Erik Stevenson"

