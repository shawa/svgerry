{-# LANGUAGE OverloadedStrings#-}
module Shapes.Interpret where

import Data.Matrix (Matrix, multStd, fromLists, toLists, identity)

import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

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


hexAttrVal :: String -> AttributeValue
hexAttrVal s = stringValue $ "#" ++ ( if (s =~ hexRe) then s else "ff0000" )
                          where hexRe = "[0-9A-Fa-f]{6}" :: String

rgbAttrVal :: Int -> Int -> Int -> Double -> AttributeValue
rgbAttrVal r g b a = stringValue $ printf "rgba(%d,%d,%d,%f)" r g b a

unsafeRawAttrVal :: Show a => a -> AttributeValue
unsafeRawAttrVal = stringValue . show

toAttr :: Style -> Attribute
toAttr (StrokeWidth  w) = strokeWidth $ unsafeRawAttrVal w

toAttr (FillColour   (Hex str))       = fill   $ hexAttrVal str
toAttr (StrokeColour (Hex str))       = stroke $ hexAttrVal str
toAttr (FillColour   (RGBA r g b a))  = fill   $ rgbAttrVal r g b a
toAttr (StrokeColour (RGBA r g b a))  = stroke $ rgbAttrVal r g b a
toAttr (FillColour c)                 = fill   $ unsafeRawAttrVal c
toAttr (StrokeColour c)               = stroke $ unsafeRawAttrVal c


-- https://developer.mozilla.org/en/docs/Web/SVG/Attribute/transform
-- We've had 3x3 matrices so far; in order to compose transformations easily
-- SVG's matrix constructor only takes 6 arguments, as the last row is always [0 0 1]
toAttrs t = transform $ matrix a b c d e f
                        where [[a, c, e], [b, d, f], [0, 0, 1]] = toLists $ getMatrix t
                                                      -- nice sanity check
toSvg :: Figure -> Svg
toSvg (styles, transforms, shape) = foldl (!) (toSvgElem shape) $ [toAttrs transforms]
                                                               ++ map toAttr styles
                                                               ++ [customAttribute "vector-effect" "non-scaling-stroke"]
                                                                   -- fixes warped strokes due to transforms
toSvgDoc :: [Figure] -> Svg
toSvgDoc figs = svgHead $ foldl (>>) (toSvgElem Empty) (map toSvg figs)
                where svgHead = docTypeSvg ! version "1.1" ! width "100%" ! height "100%" ! viewbox "-25 -25 50 50"
