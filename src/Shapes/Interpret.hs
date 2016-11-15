{-# LANGUAGE OverloadedStrings#-}
module Shapes.Interpret where
import Data.Matrix

import Shapes.Lang

type Point = Matrix Double

-- It's a Transpose!
--
--        |a b c|
-- matrix |d e f| = Matrix ...
--        |g h i|
getMatrix :: Transform -> Matrix Double

getMatrix (Identity) = fromLists [ [ 1, 0, 0 ]
                               , [ 0, 1, 0 ]
                               , [ 0, 0, 1 ] ]

getMatrix (Scale cy cx) = fromLists [ [ cx, 0,  0 ]
                                  , [ 0,  cy, 0 ]
                                  , [ 0,  0,  1 ] ]

getMatrix (Translate dy dx) = fromLists [ [1, 0, dx ]
                            , [0, 1, dy ]
                            , [0, 0, 1  ] ]


getMatrix (Rotate angle) = fromLists [ [ (cos a),  (-sin a),  0 ]
                                        , [ (sin a),  (cos a) ,  0 ]
                                        , [ 0      ,  0       ,  1 ] ]
                                     where a          = degree2rad angle
                                           degree2rad = (* (pi/180))

getMatrix (Compose t u) = (getMatrix t) `multStd` (getMatrix u)
