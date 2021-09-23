{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Blog.Tags
import Control.Monad (foldM, forM_)
import Data.Bifunctor (second)
import Data.Binary (Binary)
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Monoid ((<>))
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Data.Typeable (Typeable)
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

buildYears :: [Item a] -> Compiler [(Year, [Item a])]
buildYears items = do
    y <- foldM addYear M.empty items
    pure $ M.toList y
  where
    addYear years item = do
        year <- getYear (itemIdentifier item)
        return $ M.insertWith (++) year [item] years

archiveCtx :: [(Year, [Item String])] -> Context String
archiveCtx years =
    listField "years" yearsCtx (sequence $ makeItem <$> years) <>
    constField "title" "Archive" <>
    blogCtx
  where
    yearsCtx =
        field "year" (pure . show . fst . itemBody) <>
        field "year-count" (pure . show .length . snd . itemBody) <>
        listFieldWith "posts" postsCtx (pure . snd . itemBody)
    postsCtx =
        dateField "date" "%B %e" <>
        defaultContext

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
            years <- (reverse . sortOn fst) <$> buildYears posts
            let ctx = archiveCtx years
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
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
            posts <- loadRecent 2 "posts/*"
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

loadRecent :: (Binary a, Typeable a) => Int -> Pattern -> Compiler [Item a]
loadRecent n p = loadAll p >>= recentFirst >>= pure . (take n)

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
