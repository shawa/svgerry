# Svgerry

A Haskell-powered shape description language and renderer.

## Usage

`stack build && stack exec svgerry-exe` will launch Svgerry on `localhost:3000`

## Shapes Language
Shape documents are specified as a list of triples, where each triple is a style sheet, a list of transforms to apply (in left to right order) and a base shape (either a `Square` or a `Circle`)

For example,
```haskell
[ ([FillColour Steelblue], [Rotate 45], Square) ]
```

Will draw a blue unit square on its side.


### Colours
The colour arguments for `FillColour`, `StrokeColour` can be specified as either a Web Colour literal, `(Hex "xxxxxx")` or `(RGBA r g b a)`, where each of `r`, `g`, `b` are integers, and `a` is a real.

### Transforms
The possible transforms are `Rotate <angle>`, `Scale <y> <x>` and `Translate <y> <x>`. Transforms are applied one after another, so their order is significant.

For example, a 45 degree rotation followed by a translation by 5 units, will translate the shape along the _rotated coordinate space_. To minimize surprise, specify transforms in the order `Translate`, then `Rotate`, then `Scale`.
