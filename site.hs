{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Text.Pandoc
import Hakyll

config :: Configuration
config = defaultConfiguration {
    destinationDirectory = "docs"
}

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith ropt wopt
  where ropt = defaultHakyllReaderOptions
        wopt = defaultHakyllWriterOptions {
          writerExtensions = writerExtensions defaultHakyllWriterOptions <> pandocExtensions,
          writerHTMLMathMethod = MathML
        }

main :: IO ()
main = hakyllWith config $ do

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["papers.md", "links.md", "404.md"]) $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- The output of Forester is hardcoded to output/
    -- so we need to just copy it into the right place
    -- Because of this, we have to build Hakyll after building Forester in the makefile
    match "output/**" $ do
        route   (gsubRoute "output/" (const ""))
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
