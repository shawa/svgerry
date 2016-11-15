module Shapes.Lang where
import Data.Matrix
import Data.Aeson
import Control.Monad.Fail

type Point = Matrix Double

point :: Double -> Double -> Point
point x y = fromList 1 3 [x, y, 1]

data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

empty, square, circle :: Shape
empty  = Empty
square = Square
circle = Circle


data Transform = Identity (Matrix Double)
               | Scale (Matrix Double)
               | Rotate (Matrix Double)
               | Translate (Matrix Double)
               | Transform (Matrix Double)
                 deriving Show

-- It's a Transpose!
--
--        |a b c|
-- matrix |d e f| = Matrix ...
--        |g h i|
identity :: Transform
identity = Identity $ fromLists [ [ 1, 0, 0 ]
                                , [ 0, 1, 0 ]
                                , [ 0, 0, 1 ] ]

scale :: Double -> Double -> Transform
scale cy cx = Scale $ fromLists [ [ cx, 0,  0 ]
                                , [ 0,  cy, 0 ]
                                , [ 0,  0,  1 ] ]

translate :: Double -> Double -> Transform
translate dy dx = Translate $ fromLists [ [1, 0, dx ]
                                        , [0, 1, dy ]
                                        , [0, 0, 1  ] ]

rotate :: Double -> Transform
rotate    angle = Rotate    $ fromLists [ [ (cos a),  (-sin a),  0 ]
                                        , [ (sin a),  (cos a) ,  0 ]
                                        , [ 0      ,  0       ,  1 ] ]
                                     where a          = degree2rad angle
                                           degree2rad = (* (pi/180))

compose :: Transform -> Transform -> Transform
compose t u = Transform $ (getMatrix t) `multStd` (getMatrix u)

getMatrix :: Transform -> Matrix Double
getMatrix (Identity  m) = m
getMatrix (Scale     m) = m
getMatrix (Rotate    m) = m
getMatrix (Translate m) = m
getMatrix (Transform m) = m

transform :: Transform -> Point -> Point
transform t p  = (getMatrix t) `multStd` p

data Figure = Figure (Transform, Shape)
type Drawing = [Figure]
