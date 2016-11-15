module Shapes.Lang where

data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

data Transform = Identity
               | Scale Double Double
               | Rotate Double
               | Translate Double Double
               | Compose Transform Transform
                 deriving (Show, Read)

type Figure = (Transform, Shape)
