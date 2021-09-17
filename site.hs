{-# LANGUAGE OverloadedStrings #-}

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
            >>= loadAndApplyTemplate "templates/default.html" defaultContext'
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives" <>
                    defaultContext'

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let firstPosts = take 2 posts
                listCtx =
                    teaserField "excerpt" "content" <>
                    postCtx
                indexCtx =
                    listField "posts" listCtx (return firstPosts) <>
                    defaultContext'

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

defaultContext' :: Context String
defaultContext' =
    functionField "include" includeFiles <>
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext'

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
        { writerHighlightStyle = Just pandocCodeStyle
        }

includeFiles :: [String] -> Item a -> Compiler String
includeFiles files _ = compilerUnsafeIO $ do
    contents <- sequence $ fmap readFile files
    return $ concat contents
