module Odin.Styles (buttonPainter) where

import Gelatin.SDL2

import Odin.Core.Common
import Odin.Scripts.Button

textColorForButtonState :: ButtonState -> V4 Float
textColorForButtonState ButtonStateUp   = hex 0x333333FF
textColorForButtonState ButtonStateOver = V4 0.74 0.23 0.22 1
textColorForButtonState ButtonStateDown = red --V4 0.74 0.23 0.22 1

bgColorForButtonState :: ButtonState -> V4 Float
bgColorForButtonState ButtonStateUp   = hex 0xFFFFFFFF
bgColorForButtonState ButtonStateOver = hex 0xFFFFFFFF
bgColorForButtonState ButtonStateDown = hex 0xFFFFFFFF

bgOffsetForButtonState :: ButtonState -> V2 Float
bgOffsetForButtonState ButtonStateUp   = V2 0 0
bgOffsetForButtonState ButtonStateOver = V2 0 0
bgOffsetForButtonState ButtonStateDown = V2 2 2

buttonPainter :: FontData -> String -> Float -> ButtonState -> Pic
buttonPainter font str px btn = do
  let text = withLetters $ filled font 128 px str $
               solid $ textColorForButtonState btn
      tsz  = pictureSize text
      txy  = V2 0 5
      pad  = V2 4 4
      sz   = tsz + 2*pad
      shxy = V2 4 4
      bgxy = bgOffsetForButtonState btn
  move shxy $ withColor $ rectangle 0 sz $ const $ V4 0 0 0 0.4
  move bgxy $ withColor $ rectangle 0 sz $ const $ bgColorForButtonState btn
  move (V2 0 16 - txy + pad + bgxy) text
