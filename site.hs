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

  -- Fonts
  match "css/fonts/**" $ do
    route idRoute
    compile copyFileCompiler

  -- Hakyll templates
  match "templates/*" $ do
    compile templateBodyCompiler

  -- Root directory markdown files with no special handling
  match (fromList ["papers.md", "links.md", "404.md"]) $ do
    route $ setExtension "html"
    compile $ do
      pandoc          <- getPandocCompiler
      defaultTemplate <- loadAndApplyTemplate "templates/default.html" defaultContext pandoc
      relativizeUrls defaultTemplate

  -- The "about this site" page has its source at README.md to trick Github into displaying it
  -- We need to rename it to something more reasonable
  match "README.md" $ do
    route $ constRoute "about.html"
    compile $ do
      pandoc          <- getPandocCompiler
      defaultTemplate <- loadAndApplyTemplate "templates/default.html" defaultContext pandoc
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
  match "blog.md" $ do
    route $ setExtension "html"
    compile $ do
      allPosts         <- loadAll "posts/*"
      sortedPosts      <- recentFirst allPosts
      let postListContext = listField "posts" postContext (pure sortedPosts) <> defaultContext
      pandoc           <- getPandocCompiler
      postListTemplate <- applyAsTemplate postListContext pandoc
      defaultTemplate  <- loadAndApplyTemplate "templates/default.html" postListContext postListTemplate
      relativizeUrls defaultTemplate

  -- Index page
  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      allPosts         <- loadAll "posts/*"
      sortedPosts      <- recentFirst allPosts
      let recentPosts = take 10 sortedPosts
      let postListContext = listField "posts" postContext (pure recentPosts) <> defaultContext
      pandoc           <- getPandocCompiler
      postListTemplate <- applyAsTemplate postListContext pandoc
      defaultTemplate  <- loadAndApplyTemplate "templates/default.html" postListContext postListTemplate
      relativizeUrls defaultTemplate
