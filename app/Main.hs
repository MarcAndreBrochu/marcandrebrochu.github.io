{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Blog.Tags
import Codec.Picture (readImage, savePngImage, encodeDynamicPng, imageWidth, imageHeight)
import Codec.Picture.Types (DynamicImage(..), Image)
import Control.Exception (IOException)
import Control.Exception.Safe (tryIO)
import Control.Monad (foldM, forM_, when)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT, throwE)
import Data.Bifunctor (second)
import Data.Binary (Binary)
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (sortOn)
import Data.List.Extra (escapeHTML)
import Data.Map qualified as M
import Data.Monoid ((<>))
import Data.Text qualified as T
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Data.Tuple.Extra (both)
import Data.Typeable (Typeable)
import Hakyll
import System.Directory (removeFile, doesFileExist)
import System.Exit
import System.FilePath ((</>), (<.>))
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import Text.Pandoc.Definition (Pandoc, Inline(Math, RawInline), Format(Format), MathType(..))
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))
import Text.Pandoc.Walk (walkM)

hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . pure

dims :: Image a -> (Int,Int)
dims = (,) <$> imageWidth <*> imageHeight

dimensions :: DynamicImage -> (Int,Int)
dimensions (ImageRGB8 i)   = dims i
dimensions (ImageRGBA8 i)  = dims i
dimensions (ImageRGB16 i)  = dims i
dimensions (ImageRGBA16 i) = dims i
dimensions (ImageY8 i)     = dims i
dimensions (ImageY16 i)    = dims i
dimensions (ImageYA8 i)    = dims i
dimensions (ImageYA16 i)   = dims i
dimensions _               = error "Unsupported image format somehow!"

imageToURI :: DynamicImage -> Either RenderError String
imageToURI img =
    case encodeDynamicPng img of
        Left e -> Left $ ImageURIError e
        Right bs -> Right $ "data:image/png;base64," ++ BS.unpack (B64.encode bs)

imageToHTML :: DynamicImage -> String -> RenderEnvironment -> Either RenderError Inline
imageToHTML img code renderEnv = do
    uri <- imageToURI img
    let (w, h) = (`div` 8) `both` dimensions img
    pure $ RawInline (Format "html") $ T.pack $ concat
            [ "<img "
            , " src=\"", uri, "\""
            , " width=\"", show w, "\""
            , " height=\"", show h, "\""
            , " alt=\"", escapeHTML code, "\""
            , " class=\"", classOf renderEnv, "\""
            , "/>"
            ]
  where
    classOf REInlineMath = "inline-math"
    classOf REDisplayMath = "display-math"

transformInline :: Inline -> ExceptT RenderError IO Inline
transformInline (Math t s) = do
    let renderOptions = RenderOptions "" (toEnv t) 800
    img <- renderLatex renderOptions (T.unpack s)
    hoistEither $ imageToHTML img (T.unpack s) (toEnv t)
  where
    toEnv InlineMath = REInlineMath
    toEnv DisplayMath = REDisplayMath
transformInline x = pure x

runTransformInline :: Inline -> IO Inline
runTransformInline i = do
    res <- runExceptT $ transformInline i
    case res of
        Left err -> do
            putStrLn $ unlines ["error!!!!!!!!!!!!", show err]
            pure i
        Right i' -> pure i'

compileAllMath :: Pandoc -> Compiler Pandoc
compileAllMath = unsafeCompiler . (walkM runTransformInline)

data RenderError
    = EmptyImage
    | LatexFailure String
    | DVIPSFailure String
    | ConvertFailure String
    | IOException IOException
    | ImageReadError String
    | ImageURIError String
    deriving (Show)

data RenderEnvironment
    = REInlineMath
    | REDisplayMath

instance Show RenderEnvironment where
    show REInlineMath = "math"
    show REDisplayMath = "displaymath"

data RenderOptions = RenderOptions
    { renderOptionsPreamble :: String
    , renderOptionsEnvironment :: RenderEnvironment
    , renderOptionsDPI :: Int
    }

latexDocument :: RenderOptions -> String -> String
latexDocument renderOptions fragment =
    unlines
        [ "\\documentclass[12pt]{article}"
        , "\\pagestyle{empty}"
        , renderOptionsPreamble renderOptions
        , "\\begin{document}"
        , "\\begin{" ++ environment ++ "}"
        , fragment
        , "\\end{" ++ environment ++ "}"
        , "\\end{document}"
        ]
  where
    environment = (show . renderOptionsEnvironment) renderOptions

renderLatex :: RenderOptions -> String -> ExceptT RenderError IO DynamicImage
renderLatex renderOptions latex =
    withSystemTempDirectory "hakyll-latex" $ \tempDir -> do
        let filename = tempDir </> "working"
            tex = filename <.> "tex"
            aux = filename <.> "aux"
            dvi = filename <.> "dvi"
            png = filename <.> "png"
            ps  = filename <.> "ps"
            convertParams =
                [ "-density", show $ renderOptionsDPI renderOptions
                , "-bordercolor", "none"
                , "-border", "1x1"
                , "-trim"
                , "-background", "none"
                , "-splice", "1x0"
                , ps
                , png
                ]
        io $ writeFile tex $ latexDocument renderOptions latex
        (c, o, e) <- io $ readProcessWithExitCode "latex" ["-output-directory", tempDir, tex] ""
        io $ removeFile tex
        io $ removeFile aux
        when (c /= ExitSuccess) $ do
            io $ do
                exists <- doesFileExist dvi
                when exists $ removeFile dvi
            throwE $ LatexFailure $ unlines [o, e]
        (c', _, e') <- io $ readProcessWithExitCode "dvips" ["-q", "-E", "-o", ps, dvi] ""
        io $ removeFile dvi
        when (c' /= ExitSuccess) $ do
            throwE $ DVIPSFailure e'
        (c'', o'', e'') <- io $ readProcessWithExitCode "convert" convertParams ""
        io $ removeFile ps
        when (c'' /= ExitSuccess) $ do
            throwE $ ConvertFailure $ unlines [o'', e'']
        imgM <- io $ readImage png
        img <- withExceptT ImageReadError $ hoistEither imgM
        io $ removeFile png
        pure img
  where
    io = withExceptT IOException . ExceptT . tryIO

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
main = do
    hakyllWith config $ do
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
loadRecent n p = loadAll p
    >>= recentFirst
    >>= pure . (take n)

blogCtx :: Context String
blogCtx =
    snippetField <>
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    blogCtx

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
        { writerHighlightStyle = Just pandocCodeStyle
        }
    compileAllMath
