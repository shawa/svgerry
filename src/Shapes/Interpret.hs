{-# LANGUAGE OverloadedStrings#-}
module Shapes.Interpret where

import Data.Matrix (Matrix, multStd, fromLists, toLists, identity)

import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes hiding (multStd)
import Text.Printf (printf)

import Shapes.Lang
import Shapes.Colours

matList :: Transform -> [[Double]]
matList (Identity)      = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
matList (Scale y x)     = [[x, 0, 0], [0, y, 0], [0, 0, 1]]
matList (Translate y x) = [[1, 0, x], [0, 1, y], [0, 0, 1]]
matList (Rotate a')     = [[cos a, -sin a, 0], [sin a, cos a, 0], [0, 0, 1]]
                          where a       = deg2rad a'
                                deg2rad = (* (pi/180))

getMatrix :: [Transform] -> Matrix Double
getMatrix ts = foldl multStd (identity 3) (map mat ts)
               where mat = (fromLists . matList)

toSvgElem :: Shape -> Svg
toSvgElem Empty  = circle ! r "0"
toSvgElem Circle = circle ! r "1"
toSvgElem Square = rect   ! width  "1" ! height "1"


toAttr :: Style -> Attribute
toAttr (StrokeWidth  w) = strokeWidth $ stringAttrVal w

toAttr (StrokeColour (RGBA r g b a)) = stroke $ stringValue$ printf "rgba(%d,%d,%d,%d)" r g b a
toAttr (StrokeColour(Hex r g b)) = stroke $ stringValue $ printf "#%2x%2x%2x" r g b
toAttr (StrokeColour c)              = stroke $ stringAttrVal c

toAttr (FillColour (Hex r g b)) = fill $ stringValue $ printf "#%2x%2x%2x" r g b
toAttr (FillColour(RGBA r g b a)) = fill$ stringValue$ printf "rgba(%d,%d,%d,%d)" r g b a
toAttr (FillColour c)            = fill $ stringAttrVal c

stringAttrVal :: Show a => a -> AttributeValue
stringAttrVal = stringValue . show

-- https://developer.mozilla.org/en/docs/Web/SVG/Attribute/transform
-- We've had 3x3 matrices so far; in order to compose transformations easily
-- SVG's matrix constructor only takes 6 arguments, as the last row is always [0 0 1]
toAttrs t = transform $ matrix a b c d e f
                        where [[a, c, e], [b, d, f], [0, 0, 1]] = toLists $ getMatrix t
                                                      --nice sanity check


toSvg :: Figure -> Svg
toSvg (styles, transforms, shape) = foldl (!) (toSvgElem shape) $ [toAttrs transforms] ++ map toAttr styles

toSvgDoc :: [Figure] -> Svg
toSvgDoc figs = svgHead $ foldl (>>) (toSvgElem Empty) (map toSvg figs)
                where svgHead = docTypeSvg
                                ! version "1.1"
                                ! width "1200"
                                ! height "550"
                                ! viewbox "0 0 3 2"
