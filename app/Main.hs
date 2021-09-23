{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Debug.Trace

import Blog.Tags
import Control.Monad (foldM, forM_)
import Data.Bifunctor (second)
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

data Years = Years
    { yearsMap :: [(String, [Identifier])]
    , yearsMakeId :: String -> Identifier
    }

getYear :: (MonadMetadata m, MonadFail m) => Identifier -> m String
getYear id = do
    timeUTC <- getItemUTC locale id
    return $ formatTime locale "%Y" timeUTC
  where
    locale = defaultTimeLocale

buildYears :: (MonadMetadata m, MonadFail m) => Pattern -> (String -> Identifier) -> m Years
buildYears pattern makeId = do
    ids <- getMatches pattern
    yearsMap <- foldM addYears M.empty ids
    return $ Years (M.toList yearsMap) makeId
  where
    addYears yearsMap id' = do
        year <- getYear id'
        return $ M.insertWith (++) year [id'] yearsMap

yearsRules :: Years -> (String -> Pattern -> Rules ()) -> Rules ()
yearsRules years rules = do
    forM_ (yearsMap years) $ \(year, identifiers) ->
        create [yearsMakeId years year] $
            rules year (fromList identifiers)

recentYearsFirst :: [Item String] -> Compiler [Item String]
recentYearsFirst = do
    pure

makeYearsScore :: Years -> M.Map Identifier Int
makeYearsScore years = foldr addYear M.empty keys
  where
    keys :: [String]
    keys = fmap fst (yearsMap years)

    makeId :: String -> Identifier
    makeId = yearsMakeId years

    addYear :: String -> M.Map Identifier Int -> M.Map Identifier Int
    addYear n scores = M.insert (makeId n) (read n :: Int) scores

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

    years <- buildYears "posts/*" (fromCapture "years/*")

    traceShow (makeYearsScore years) $ return ()

    yearsRules years $ \year pattern -> compile $ do
        posts <- recentFirst =<< loadAll pattern
        let yearCtx =
                listField "posts" postCtx (return posts) <>
                constField "year" year <>
                blogCtx
        makeItem ""
            >>= loadAndApplyTemplate "templates/year.html" yearCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            x <- loadAll "years/*"
                    >>= recentYearsFirst
                    >>= pure . concat . (fmap itemBody)
            let ctx = constField "title" "Archive" <> blogCtx
                yearsCtx =
                    listField "years"
                        (field "year" yearName <> field "year-count" yearCount)
                        (traverse (makeItem . (second length)) (yearsMap years)) <>
                    constField "contents" x <>
                    ctx
                  where
                    yearName = pure . fst . itemBody
                    yearCount = pure . show . snd . itemBody
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" yearsCtx
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
