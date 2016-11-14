import Data.Matrix

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
               | Compose Transform Transform
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
                                     where a = angle

transformMatrix :: Transform -> Matrix Double
transformMatrix (Identity  m) = m
transformMatrix (Scale     m) = m
transformMatrix (Rotate    m) = m
transformMatrix (Translate m) = m
transformMatrix (Compose t u) = (transformMatrix t) `multStd` (transformMatrix u)

transform :: Transform -> Point -> Point
transform t p  = (transformMatrix t) `multStd` p
