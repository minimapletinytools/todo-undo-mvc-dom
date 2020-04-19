{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Relude

import           Reflex
import           Reflex.Dom

import           Data.FileEmbed

import           Todo

main :: IO ()
main = do
  let
    -- TODO figure out how to do this without embedding
    -- TODO get a better potato.css
    css = $(embedStringFile "potato.css")
  mainWidgetWithCss css $ do todoWidget
