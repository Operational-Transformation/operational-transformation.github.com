{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Hakyll

main :: IO ()
main = hakyll $ do
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler
  match "static/bootstrap/css/bootstrap.min.css" $ do
    route idRoute
    compile copyFileCompiler
  match "templates/*" $ compile templateCompiler
  match "*.rst" $ do
    route   $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/layout.hamlet"
      >>> relativizeUrlsCompiler
