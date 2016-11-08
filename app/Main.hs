{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes


import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Text

main = scotty 3000 $ do
    get "/" $ do
      S.html . renderHtml $ do
        h1 "Heading right here"
