{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Data.String (fromString)
import Hakyll

main :: IO ()
main = hakyllWith config $ do
  match "less/*.less" $ do
    route $ setExtension "css" `composeRoutes` gsubRoute "less/" (const "css/")
    compile $ getResourceString >>> unixFilter "lessc" ["--compress", "-"]
  match "coffee/*.coffee" $ do
    route $ setExtension "js" `composeRoutes` gsubRoute "coffee/" (const "js/")
    compile $ getResourceString >>> unixFilter "coffee" ["--stdio", "--print"]
  match "templates/*" $ compile templateCompiler
  let components = map (fromString . ("components/" ++))
        [ "jquery/jquery.js"
        , "underscore/underscore.js"
        , "codemirror/lib/codemirror.css"
        , "codemirror/lib/codemirror.js"
        , "d3/d3.v2.js"
        , "ot/dist/ot.js"
        , "ot/lib/server.js"
        , "bootstrap/js/bootstrap-tooltip.js"
        , "bootstrap/js/bootstrap-popover.js"
        ]
  match (list components) $ do
    route $ gsubRoute "components/" (const "static/")
    compile copyFileCompiler
  match "src/*.rst" $ do
    route   $ replaceSrc `composeRoutes` setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/layout.hamlet"
      >>> relativizeUrlsCompiler
  match "src/*.js" $ do
    route replaceSrc
    compile copyFileCompiler
  match "src/*.html" $ do
    route replaceSrc
    compile $ readPageCompiler
      >>> addDefaultFields
      >>> applyTemplateCompiler "templates/layout.hamlet"
      >>> relativizeUrlsCompiler
  where
    replaceSrc = gsubRoute "src/" (const "")
    config = defaultHakyllConfiguration
      { destinationDirectory = "."
      }