{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Styles (buttonPainter) where

import           Gelatin.SDL2
import           Gelatin.Fruity
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad (when)

import Odin.GUI.Button
import Odin.GUI.TextInput.Internal
--------------------------------------------------------------------------------
-- Button Styling
--------------------------------------------------------------------------------
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

buttonPainter :: (ButtonData, ButtonState) -> ColorPicture ()
buttonPainter (ButtonData{..}, st) = do
  let text :: ColorPicture ()
      text = coloredString btnDataFont 72 btnDataPointSize btnDataStr $
               const $ textColorForButtonState st
      tsz  = pictureSize' text
      pad  = V2 4 4
      sz = tsz + 2*pad
      shxy = V2 4 4
      bgxy = bgOffsetForButtonState st

  -- drop shadow
  embed $ setGeometry $ geometry $ add $ fan $
    mapVertices (,V4 0 0 0 0.4) $ vertices $ rectangle shxy (shxy + sz)
  -- background
  embed $ setGeometry $ geometry $ add $ fan $
    mapVertices (,bgColorForButtonState st) $ vertices $
      rectangle bgxy (bgxy + sz)
  -- button text
  embed $ do
    move $ (V2 0 btnDataPointSize) + (V2 4 0) + bgxy
    text
--------------------------------------------------------------------------------
-- TextInput Styling
--------------------------------------------------------------------------------
textColorForTextInputState :: TextInputState -> V4 Float
textColorForTextInputState st = if st == TextInputStateEditing then canary else white

bgColorForTextInputState :: TextInputState -> V4 Float
bgColorForTextInputState TextInputStateDown = V4 0.3 0.3 0.3 1
bgColorForTextInputState TextInputStateEditing = V4 0.2 0.2 0.2 1
bgColorForTextInputState _ = V4 1 1 1 0

lnColorForTextInputState :: TextInputState -> V4 Float
lnColorForTextInputState TextInputStateUp = white `withAlpha` 0.4
lnColorForTextInputState TextInputStateOver = white `withAlpha` 0.8
lnColorForTextInputState TextInputStateDown = white
lnColorForTextInputState _ = white `withAlpha` 0.8

textInputPainter :: (TextInputData, TextInputState) -> ColorPicture ()
textInputPainter (TextInputData{..}, st) = do
  background
  foreground
  where textcolor= textColorForTextInputState st
        px = txtnPointSize txt
        txttxt = txtnText txt
        drawText = coloredString (txtnFont txt) 72 px (T.unpack txttxt) $ const textcolor
        leaderInc = if hasLeader then V2 inc 0 else 0
        endSpaces = T.length $ T.takeWhile (== ' ') $ T.reverse txttxt
        spaceInc = V2 (px/2) 0 ^* fromIntegral endSpaces
        V2 w h = sum [pictureSize' drawText, leaderInc, spaceInc]
        size@(V2 tw th) = V2 (max (px/2) w) (max px h)
        bar = setGeometry $ geometry $ add $ fan $
          mapVertices (,textcolor `withAlpha` 0.5) $ vertices $
            rectangle (V2 tw padding) (V2 (tw + 1.5) (th + padding))

        hasLeader = st == TextInputStateEditing
        padding = 4
        inc = 1.5 * padding
        bgcolor = bgColorForTextInputState st
        lncolor = lnColorForTextInputState st
        foreground = do
          embed $ do
            move (V2 0 th + V2 padding padding)
            drawText
          when hasLeader $ embed bar
        background = do--withColor $ rectangle 0 (sz + V2 inc inc) $ const bgcolor
          setStroke [StrokeWidth 3, StrokeFeather 1]
          setGeometry $ geometry $ do
            add $ fan $ mapVertices (,bgcolor) $ vertices $
              rectangle 0 (size + V2 inc inc)
            add $ line $ vertices $ do
             to (0, lncolor)
             to (V2 (tw + inc) 0, lncolor)
             to (V2 (tw + inc) (th + inc), lncolor)
             to (V2 0 (th + inc), lncolor)
             to (0, lncolor)
