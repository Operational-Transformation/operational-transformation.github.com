{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Data.String (fromString)
import Hakyll
import Data.Default (Default (..))
import Text.Pandoc.Options (Extension (..), ReaderOptions (..))
import Data.Monoid ((<>))
import qualified Data.Set as S

main :: IO ()
main = hakyll $ do
  match "less/*.less" $ do
    route $ setExtension "css" `composeRoutes` gsubRoute "less/" (const "css/")
    compile $ getResourceLBS >>= withItemBody (unixFilterLBS "lessc" ["--compress", "-"])
  match "coffee/*.coffee" $ do
    route $ setExtension "js" `composeRoutes` gsubRoute "coffee/" (const "js/")
    compile $ getResourceLBS >>= withItemBody (unixFilterLBS "coffee" ["--stdio", "--print"])
  match "templates/*" $ compile templateCompiler
  let components = map (fromString . ("bower_components/" ++))
        [ "jquery/dist/jquery.js"
        , "underscore/underscore.js"
        , "codemirror/lib/codemirror.css"
        , "codemirror/lib/codemirror.js"
        , "d3/d3.js"
        , "ot/dist/ot.js"
        , "ot/lib/server.js"
        , "ot/lib/simple-text-operation.js"
        , "bootstrap/js/tooltip.js"
        , "bootstrap/js/popover.js"
        ]
  match (fromList components) $ do
    route $ gsubRoute "bower_components/" (const "static/")
    compile copyFileCompiler
  match "src/*.markdown" $ do
    route   $ replaceSrc `composeRoutes` setExtension "html"
    compile $ do
      tpl <- loadBody "templates/layout.html"
      let rExts = S.insert Ext_raw_html $
                  S.delete Ext_markdown_in_html_blocks $
                  readerExtensions defaultHakyllReaderOptions
          readerOptions = defaultHakyllReaderOptions { readerExtensions = rExts }
      pandocCompilerWith readerOptions defaultHakyllWriterOptions
        >>= applyTemplate tpl defaultContext
        >>= relativizeUrls
  match "src/*.js" $ do
    route replaceSrc
    compile copyFileCompiler
  match "src/*.html" $ do
    route replaceSrc
    compile $ do
      tpl <- loadBody "templates/layout.html"
      pandocCompilerWith (def { readerParseRaw = True }) def
        >>= applyTemplate tpl defaultContext
        >>= relativizeUrls
  where
    replaceSrc = gsubRoute "src/" (const "")
