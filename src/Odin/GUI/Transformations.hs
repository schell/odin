module Odin.GUI.Transformations where

import Odin.Core
import Linear
import Gelatin (Affine(..))

move :: Float -> Float -> RenderTransform
move x y = Spatial $ Translate $ V2 x y

scale :: Float -> Float -> RenderTransform
scale x y = Spatial $ Scale $ V2 x y

rotate :: Float -> RenderTransform
rotate = Spatial . Rotate

multiply :: Float -> Float -> Float -> Float -> RenderTransform
multiply r g b a = Multiply $ V4 r g b a

redChannelReplacement :: Float -> Float -> Float -> Float -> RenderTransform
redChannelReplacement r g b a = ColorReplacement $ V4 r g b a
