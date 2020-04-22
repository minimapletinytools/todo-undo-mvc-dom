{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Relude

import           Reflex.Dom

import           Data.FileEmbed

import           Todo

main :: IO ()
main = do
  let css = $(embedFile "potato.css")
  mainWidgetWithCss css $ todoWidget
