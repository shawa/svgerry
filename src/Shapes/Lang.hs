module Shapes.Lang where
import Shapes.Colours (Colour)

data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

data Transform = Identity
               | Scale Double Double
               | Rotate Double
               | Translate Double Double
                 deriving (Show, Read)

data Style = StrokeWidth Double
           | StrokeColour Colour
           | FillColour Colour
             deriving (Show, Read)

type Figure = ([Style], [Transform], Shape)
