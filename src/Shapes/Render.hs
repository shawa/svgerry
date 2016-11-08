{-# LANGUAGE OverloadedStrings#-}
module Shapes.Render where

import Shapes.Lang

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A


toSvg :: Drawing -> S.Svg
toSvg [(_, square)] = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2" $ do
  S.g $ do
    S.rect ! A.width "1" ! A.height "2" ! A.fill "#03"
