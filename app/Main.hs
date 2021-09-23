{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Debug.Trace

import Blog.Tags
import Control.Monad (foldM, forM_)
import Data.Bifunctor (second)
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Monoid ((<>))
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Hakyll
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , providerDirectory = "site"
    }

type Year = Int

getYear :: (MonadMetadata m, MonadFail m) => Identifier -> m Year
getYear articleId = do
    timeUTC <- getItemUTC locale articleId
    return $ read $ formatTime locale "%Y" timeUTC
  where
    locale = defaultTimeLocale

data Article = Article
    { articleDate :: String
    , articleUrl :: String
    , articleTitle :: String
    , articleId :: Identifier
    }

buildYears :: [Item String] -> Compiler [(Year, [Article])]
buildYears items = do
    let ids = itemIdentifier <$> items
    y <- foldM addYear M.empty ids
    pure $ M.toList y
  where
    addYear years id = do
        year <- getYear id
        title <- getMetadataField' id "title"
        utc <- getItemUTC defaultTimeLocale id
        let empty' = fail $ "No route url found for item " ++ show id
        route <- fmap (maybe empty' toUrl) $ getRoute id
        let article = Article
                    { articleDate = formatTime defaultTimeLocale "%F" utc
                    , articleUrl = route
                    , articleTitle = title
                    , articleId = id
                    }
        return $ M.insertWith (++) year [article] years

sortYears :: [(Year, a)] -> [(Year, a)]
sortYears = reverse . sortOn fst

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ do
            makeItem $ styleToCss pandocCodeStyle

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            years' <- buildYears posts
            let years = sortYears years'
            -- [(year, [article info])] (sorted starting at 2021, 2020, 2019, ...)
            -- year (String) -> [article info]
            -- article info (???) -> date, title, url
            let ctx = constField "title" "Archive" <> blogCtx
                archiveCtx =
                    listField "years"
                        (field "year" (return . show . fst . itemBody) <>
                         field "year-count" (pure . show . length . snd . itemBody) <>
                         listFieldWith "posts"
                            (field "date" (return . articleDate . itemBody) <>
                             field "url" (return . articleUrl . itemBody) <>
                             field "title" (return . articleTitle . itemBody))
                            (\item -> sequence $ makeItem <$> (snd . itemBody) item)
                        )
                        (sequence $ makeItem <$> years)
                    <> ctx
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Builds a map from tags to posts with that tag.
    tags <- buildTagsWith getNormalizedTags "posts/*" (fromCapture "tags/*")

    tagsRules tags $ \tag pattern -> compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx =
                listField "posts" postCtx (return posts) <>
                constField "tag" tag <>
                blogCtx
        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" ctx
            >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        compile $ do
            x <- loadAll "tags/*" >>= pure . concat . (fmap itemBody)
            let ctx = constField "title" "Tags" <> blogCtx
                tagsCtx =
                    listField "tags"
                        (field "tag" tagName <> field "tag-count" tagCount)
                        (traverse (makeItem . (second length)) (tagsMap tags)) <>
                    constField "contents" x <>
                    ctx
                  where
                    tagName = pure . fst . itemBody
                    tagCount = pure . show . snd . itemBody
            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            pandocCompiler'
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplateWithTags "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let recentPosts n = take n <$> (loadAll "posts/*" >>= recentFirst)
            posts <- recentPosts 2
            let listCtx =
                    teaserField "excerpt" "content" <>
                    postCtx
                indexCtx =
                    listField "posts" listCtx (return posts) <>
                    blogCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "snippets/*" $ compile getResourceBody

blogCtx :: Context String
blogCtx =
    snippetField <>
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    blogCtx

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
        { writerHighlightStyle = Just pandocCodeStyle
        }
