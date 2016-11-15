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


main = scotty 3000 $ do
    get "/" $ do file "./static/index.html"

    get "/svg" $ do
      shapeText <- (S.param "shapeText") `rescue` return
      S.setHeader "Content-Type" "image/svg+xml"
      let shapeDrawing = (read $ T.unpack $  TL.toStrict shapeText) :: [Figure]
      S.text $ renderSvg $ toSvgDoc $ shapeDrawing
