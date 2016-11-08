{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes


import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Svg.Renderer.Utf8

import Shapes.Lang
import Shapes.Render


exampleShape :: Shape
exampleShape = read "square"

exampleDrawing :: Drawing

exampleDrawing = [(identity, exampleShape)]

main = scotty 3000 $ do
    get "/" $ do
      S.setHeader "Content-Type" "image/svg+xml"
      S.raw $ renderSvg $ toSvg exampleDrawing
