{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes


import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Svg.Renderer.Text

import Text.Read hiding (get)

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Shapes.Lang
import Shapes.Interpret


shapeFromText :: String -> Shape
shapeFromText string = case readMaybe string of Just shape -> shape
                                                Nothing    -> square

main = scotty 3000 $ do
    get "/" $ do file "./static/index.html"

    get "/svg" $ do
      shapeText <- (S.param "shapeText") `rescue` return
      S.setHeader "Content-Type" "image/svg+xml"
      let shape = shapeFromText $ (T.unpack . TL.toStrict) shapeText
      S.text "Hello"
