{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Hakyll

main :: IO ()
main = hakyllWith config $ do
  match "src/css/*" $ do
    route $ idRoute `composeRoutes` replaceSrc
    compile compressCssCompiler
  match "src/static/bootstrap/css/bootstrap.min.css" $ do
    route $ idRoute `composeRoutes` replaceSrc
    compile copyFileCompiler
  match "src/templates/*" $ compile templateCompiler
  match "src/*.rst" $ do
    route   $ replaceSrc `composeRoutes` setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "src/templates/layout.hamlet"
      >>> relativizeUrlsCompiler
  where
    replaceSrc = gsubRoute "src/" (const "")
    config = defaultHakyllConfiguration
      { destinationDirectory = "."
      }