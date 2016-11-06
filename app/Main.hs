{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Web.Scotty as S

import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text


response name = do renderHtml $ do
                     h1 ("Hello " >> toHtml name)


main = S.scotty 3000 $ do
  S.get "/" $ do
    S.html "Hello World!"

  S.get "/greet/:name" $ do
    name <- S.param "name"
    S.html $ response name
