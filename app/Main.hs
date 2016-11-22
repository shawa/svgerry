{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Data.Text.Lazy (toStrict)
import Data.Text (unpack)
import Shapes.Lang (Figure)
import Shapes.Interpret (toSvgDoc)

import Web.Scotty

main = scotty 3000 $ do
    get "/" $ do file "./static/index.html"

    get "/svg" $ do
      setHeader "Content-Type" "image/svg+xml"
      shapeText <- (param "shapeText") `rescue` return
      let shapeDrawing = (read $ unpack $  toStrict shapeText) :: [Figure]
      raw $ renderSvg $ toSvgDoc $ shapeDrawing
