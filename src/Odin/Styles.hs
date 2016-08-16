{-# LANGUAGE TupleSections #-}
module Odin.Styles (buttonPainter) where

import Gelatin.SDL2
import Gelatin.Fruity

import Odin.Scripts.Button

textColorForButtonState :: ButtonState -> V4 Float
textColorForButtonState ButtonStateUp   = fromHex 0x333333FF
textColorForButtonState ButtonStateOver = V4 0.74 0.23 0.22 1
textColorForButtonState ButtonStateDown = red --V4 0.74 0.23 0.22 1
textColorForButtonState _ = textColorForButtonState ButtonStateUp

bgColorForButtonState :: ButtonState -> V4 Float
bgColorForButtonState ButtonStateUp   = fromHex 0xFFFFFFFF
bgColorForButtonState ButtonStateOver = fromHex 0xFFFFFFFF
bgColorForButtonState ButtonStateDown = fromHex 0xFFFFFFFF
bgColorForButtonState _ = bgColorForButtonState ButtonStateUp

bgOffsetForButtonState :: ButtonState -> V2 Float
bgOffsetForButtonState ButtonStateUp   = V2 0 0
bgOffsetForButtonState ButtonStateOver = V2 0 0
bgOffsetForButtonState ButtonStateDown = V2 2 2
bgOffsetForButtonState _ = bgOffsetForButtonState ButtonStateUp

buttonPainter :: Font -> String -> Float -> ButtonState -> ColorPicture ()
buttonPainter font str px btn = do
  let text :: ColorPicture ()
      text = coloredString font 72 px str $ const $ textColorForButtonState btn
      tsz  = pictureSize' text
      pad  = V2 4 4
      sz = tsz + 2*pad
      shxy = V2 4 4
      bgxy = bgOffsetForButtonState btn

  -- drop shadow
  embed $ setGeometry $ geometry $ add $ fan $
    mapVertices (,V4 0 0 0 0.4) $ vertices $ rectangle shxy (shxy + sz)
  -- background
  embed $ setGeometry $ geometry $ add $ fan $
    mapVertices (,bgColorForButtonState btn) $ vertices $
      rectangle bgxy (bgxy + sz)
  -- button text
  embed $ do
    move $ (V2 0 px) + (V2 4 0) + bgxy
    text
