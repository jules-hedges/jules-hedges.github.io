{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc
import Hakyll

-- Custom Hakyll config
config :: Configuration
config = defaultConfiguration {
  -- Github Pages has a hardcoded requirement that the site is in docs/
  destinationDirectory = "docs"
}

-- Custom Pandoc settings
getPandocCompiler :: Compiler (Item String)
getPandocCompiler = pandocCompilerWith readerOptions writerOptions where
  readerOptions = defaultHakyllReaderOptions
  writerOptions = defaultHakyllWriterOptions {
    -- Enables Pandoc flavoured markdown
    writerExtensions = pandocExtensions <> writerExtensions defaultHakyllWriterOptions,
    -- Use MathML for mathmode rendering (not KaTeX or MathJax!)
    writerHTMLMathMethod = MathML
  }

-- Template context for blog posts
postContext :: Context String
postContext = dateField "date" "%B %e, %Y" <> defaultContext

-- Hakyll entry point
main :: IO ()
main = hakyllWith config $ do

  -- Assorted root directory files that need to be copied verbatim
  match (fromList ["CNAME"]) $ do
    route idRoute
    compile copyFileCompiler

  -- Assets folder needs to be copied unchanged
  match "assets/**" $ do
    route idRoute
    compile copyFileCompiler

  -- The output of Forester is hardcoded to output/
  -- so we need to just copy it into the right place
  -- Because of this, we have to build Hakyll after building Forester in the makefile
  match "output/**" $ do
    route $ gsubRoute "output/" $ const ""
    compile copyFileCompiler

  -- CSS files
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- Hakyll templates
  match "templates/*" $ do
    compile templateBodyCompiler

  -- Root directory markdown files with no special handling
  match (fromList ["papers.md", "links.md", "404.md"]) $ do
    route $ setExtension "html"
    compile $ do
      pandoc          <- getPandocCompiler
      defaultTemplate <- loadAndApplyTemplate "templates/default.html" postContext pandoc
      relativizeUrls defaultTemplate

  -- All blog posts
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      pandoc          <- getPandocCompiler
      postTemplate    <- loadAndApplyTemplate "templates/post.html"    postContext pandoc
      defaultTemplate <- loadAndApplyTemplate "templates/default.html" postContext postTemplate
      relativizeUrls defaultTemplate

  -- Blog posts page
  create ["blog.html"] $ do
    route idRoute
    compile $ do
      allPosts        <- loadAll "posts/*"
      sortedPosts     <- recentFirst allPosts
      let context = listField "posts" postContext (pure sortedPosts) <> constField "title" "Blog" <> defaultContext
      newPage         <- makeItem ""
      blogTemplate    <- loadAndApplyTemplate "templates/blog.html"    context newPage
      defaultTemplate <- loadAndApplyTemplate "templates/default.html" context blogTemplate
      relativizeUrls defaultTemplate

  -- Index page
  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      allPosts        <- loadAll "posts/*"
      sortedPosts     <- recentFirst allPosts
      let recentPosts = take 10 sortedPosts
      let context = listField "posts" postContext (pure recentPosts) <> defaultContext
      pandoc          <- getPandocCompiler
      indexTemplate   <- applyAsTemplate context pandoc
      defaultTemplate <- loadAndApplyTemplate "templates/default.html" context indexTemplate
      relativizeUrls defaultTemplate
