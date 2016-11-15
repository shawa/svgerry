{-# LANGUAGE OverloadedStrings#-}
module Shapes.Interpret where

import qualified Data.Matrix as M
import Control.Monad

import Text.Blaze.Svg11 ((!), Svg)
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes

import Shapes.Lang

type Point = M.Matrix Double

getMatrix :: [Transform] -> M.Matrix Double
getMatrix [] = M.identity 3
getMatrix [t] = M.fromLists $ case t of Identity      -> [[1, 0, 0] ,[0, 1, 0] ,[0, 0, 1]]
                                        Scale y x     -> [[x, 0, 0] ,[0, y, 0] ,[0, 0, 1]]
                                        Translate y x -> [[1, 0, x] ,[0, 1, y] ,[0, 0, 1]]
                                        Rotate a'     -> [[(cos a), (-sin a), 0] ,[(sin a), (cos a) , 0] ,[0, 0, 1]]
                                         where a       = deg2rad a'
                                               deg2rad = (* (pi/180))
getMatrix (t:ts) = (getMatrix [t]) `M.multStd` (getMatrix ts)

toSvgElem :: Shape -> S.Svg
toSvgElem Empty  = S.circle ! r "0"
toSvgElem Circle = S.circle ! r "1"
toSvgElem Square = S.rect   ! width  "10" ! height "10"



-- https://developer.mozilla.org/en/docs/Web/SVG/Attribute/transform
-- We've had 3x3 matrices so far, in order to compose transformations easily
-- SVG's matrix constructor only takes 6 arguments, as the last row is always [0 0 1]
-- which looks like:
--
--       a c e
--       b d f = getMatrix t
--       0 0 1
toAttrs t = transform $ S.matrix a b c d e f
                          where  a = M.getElem 1 1 transMat
                                 b = M.getElem 2 1 transMat
                                 c = M.getElem 1 2 transMat
                                 d = M.getElem 2 2 transMat
                                 e = M.getElem 1 3 transMat
                                 f = M.getElem 2 3 transMat
                                 transMat = getMatrix t

toSvg :: Figure -> S.Svg
toSvg (t, s) = do
  toSvgElem s ! toAttrs t

toSvgs :: [Figure] -> S.Svg
toSvgs []         = toSvgElem Empty
toSvgs [fig]      = toSvg fig
toSvgs (fig:figs) = toSvg fig >> toSvgs figs


toSvgDoc :: [Figure] -> S.Svg
toSvgDoc figs = svgHead $ do
          toSvgs figs

svgHead = do
  S.docTypeSvg ! version "1.1" ! width "1200" ! height "550" ! viewbox "0 0 3 2"
