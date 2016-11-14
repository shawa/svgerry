{-# LANGUAGE OverloadedStrings#-}
module Shapes.Render where

import Shapes.Lang

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A


svgHeader = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2"


toSvg :: Drawing -> S.Svg
toSvg triples = svgHeader $ do
  mconcat $ map toSvgElem triples

toSvgElem (t, s) = do
  S.g $ do
    (getSvgElem s) ! (makeAttrs t)


getSvgElem s = case s of circle -> S.circle
                         square -> S.rect
                         _      -> S.circle
