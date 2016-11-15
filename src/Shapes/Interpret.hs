{-# LANGUAGE OverloadedStrings#-}
module Shapes.Interpret where

import qualified Data.Matrix as M
import Control.Monad

import Text.Blaze.Svg11 ((!), Svg)
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes

import Shapes.Lang

type Point = M.Matrix Double

getMatrix :: Transform -> M.Matrix Double
getMatrix (Compose t u) = (getMatrix t) `M.multStd` (getMatrix u)
getMatrix t = M.fromLists $ case t of Identity      -> [[1, 0, 0] ,[0, 1, 0] ,[0, 0, 1]]
                                      Scale y x     -> [[x, 0, 0] ,[0, y, 0] ,[0, 0, 1]]
                                      Translate y x -> [[1, 0, x] ,[0, 1, y] ,[0, 0, 1]]
                                      Rotate a'     -> [[(cos a), (-sin a), 0] ,[(sin a), (cos a) , 0] ,[0, 0, 1]]
                                       where a       = deg2rad a'
                                             deg2rad = (* (pi/180))

toSvgElem :: Shape -> S.Svg
toSvgElem Empty  = S.circle ! radius "0"
toSvgElem Circle = S.circle ! radius "1"
toSvgElem Square = S.rect   ! width  "1" ! height "1"

toAttrs t = transform $ S.matrix a b c d e f
                          where  a = M.getElem 1 1 transMat
                                 b = M.getElem 2 1 transMat
                                 c = M.getElem 1 2 transMat
                                 d = M.getElem 2 2 transMat
                                 e = M.getElem 1 2 transMat
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
  S.docTypeSvg ! version "1.1" ! width "150" ! height "100" ! viewbox "0 0 3 2"
