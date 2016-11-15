module Shapes.Lang where

data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

data Transform = Identity
               | Scale Double Double
               | Rotate Double
               | Translate Double Double
                 deriving (Show, Read)

type Colour = String

data Style = StrokeWidth Double
           | StokeColour Colour
           | FillColour Colour
             deriving (Show, Read)

type Figure = ([Transform], Shape)
