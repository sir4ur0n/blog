{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid     (mappend)
import qualified GHC.IO.Encoding as E
import           Hakyll

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about-me.markdown", "feedback.markdown"]) $ do
      route $ setExtension "html"
      compile $ pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext >>= relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler 
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend` constField "title" "Home" `mappend` defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>= loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateCompiler
    -- RSS, see https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
        renderRss myFeedConfiguration feedCtx posts

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Julien Debon's blog"
    , feedDescription = "Blog posts related to functional programming and code quality"
    , feedAuthorName  = "Julien Debon"
    , feedAuthorEmail = "julien.debon@pm.me"
    , feedRoot        = "https://sir4ur0n.github.io"
    }
