module Shapes.Lang where

data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

empty, square, circle :: Shape
empty  = Empty
square = Square
circle = Circle


data Transform = Identity
               | Scale Double Double
               | Rotate Double
               | Translate Double Double
               | Compose Transform Transform
                 deriving (Show, Read)


data Figure = Figure (Transform, Shape)
type Drawing = [Figure]
