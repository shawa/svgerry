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

data Colour = Red
            | Green
            | Blue
            | Cyan
            | Magenta
            | Yellow
            | Black
            | White
            | Gray
              deriving (Show, Read)

data Style = StrokeWidth Double
           | StrokeColour Colour
           | FillColour Colour
             deriving (Show, Read)

type Figure = ([Style], [Transform], Shape)
