{-# LANGUAGE OverloadedStrings #-}

import Data.List (intersperse)
import Data.Monoid ((<>))
import Hakyll
import Hakyll.Core.Compiler.Internal (compilerUnsafeIO)
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    }

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

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" blogCtx
            >>= relativizeUrls

    -- Builds a map from tags to posts with that tag.
    --tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            pandocCompiler'
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplateWithTags "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives" <>
                    blogCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
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

-- Only add a tags field if one is present in the metadata.
loadAndApplyTemplateWithTags :: Identifier -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplateWithTags tid ctx item = hasTags item >>= withTags
  where
    hasTags = (fmap (not . null)) . (getTags . itemIdentifier)

    withTags False = loadAndApplyTemplate tid ctx item
    withTags True  = loadAndApplyTemplate tid (tagsListField <> ctx) item

blogCtx :: Context String
blogCtx =
    snippetField <>
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    blogCtx

getNormalizedTags :: MonadMetadata m => Item a -> m [String]
getNormalizedTags item =
    fmap normalizeTag <$> (getTags . itemIdentifier) item
  where
    normalizeTag = concat . intersperse "-" . words

tagsListField :: Context a
tagsListField =
    listFieldWith "tags" (field "tag" tag) tags
  where
    tag = pure . itemBody
    tags item = getNormalizedTags item >>= (sequence . fmap makeItem)

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
        { writerHighlightStyle = Just pandocCodeStyle
        }
