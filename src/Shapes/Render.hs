{-# LANGUAGE OverloadedStrings#-}
module Shapes.Render where

import Shapes.Lang

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A


svgHeader = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2"

svgBoilerPlate shapeSvg = svgHeader $ do
  S.g shapeSvg

toSvg :: Drawing -> S.Svg
toSvg [(_, square)] = svgBoilerPlate $ do
    S.rect ! A.width "1" ! A.height "1" ! A.fill "#03"


toSvg _ = svgBoilerPlate $ do
  S.rect ! A.width "1" ! A.height "10" ! A.fill "#ff00ff"
