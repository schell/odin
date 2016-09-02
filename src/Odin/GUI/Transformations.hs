module Odin.GUI.Transformations where

import Odin.Core
import Linear
import Gelatin (Affine(..))

move :: V2 Float -> RenderTransform
move = Spatial . Translate

scale :: V2 Float -> RenderTransform
scale = Spatial . Scale

rotate :: Float -> RenderTransform
rotate = Spatial . Rotate

multiply :: V4 Float -> RenderTransform
multiply = Multiply

redChannelReplacement :: V4 Float -> RenderTransform
redChannelReplacement = ColorReplacement
