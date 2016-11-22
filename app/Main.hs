{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Data.Text.Lazy (toStrict, Text)
import Data.Text (unpack)
import Shapes.Lang (Figure)
import Shapes.Interpret (toSvgDoc)
import Text.Read (readMaybe)
import Network.HTTP.Types.Status (mkStatus)
import Web.Scotty

toShape :: Text -> Maybe [Figure]
toShape = readMaybe . unpack . toStrict

main = scotty 3000 $ do
    get "/" $ do file "./static/index.html"

    get "/svg" $ do
      shapeText <- (param "shapeText") `rescue` return
      case toShape shapeText of Just figures -> do setHeader "Content-Type" "image/svg+xml"
                                                   (raw . renderSvg . toSvgDoc) figures
                                Nothing      -> do status $ mkStatus 406 "Failed to parse Shape specification"
                                                   text "Failed to parse! Mismatched or missing parens?"
