{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Styles
  ( buttonPainter
  , textInputForegroundPainter
  , textInputBackgroundPainter
  , withDefaultButton
  , withDefaultTextInput
  ) where

import           Gelatin.SDL2
import           Gelatin.FreeType2
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)
import           Control.Monad (when)

import Odin.Core
import Odin.GUI.Button
import Odin.GUI.TextInput
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

buttonPainter :: MonadIO m => Painter (ButtonData, ButtonState) m
buttonPainter = Painter $ \(ButtonData{..}, st) -> do
  let text = freetypePicture btnDataAtlas (textColorForButtonState st) btnDataStr
  V2 tw th <- runPictureSizeT text

  let pad  = V2 4 4
      sz = V2 tw gh + 2*pad
      shxy = V2 4 4
      bgxy = bgOffsetForButtonState st
      gh = glyphHeight $ atlasGlyphSize btnDataAtlas
  return
    -- drop shadow and background
    [ColorPainting $ do
      embed $ setGeometry $ fan $
        mapVertices (,V4 0 0 0 0.4) $ rectangle shxy (shxy + sz)
      embed $ setGeometry $ fan $
        mapVertices (,bgColorForButtonState st) $ rectangle bgxy (bgxy + sz)
    -- button text
    ,TexturePainting $ do
      move $ (V2 0 gh) + (V2 4 0) + bgxy
      text
    ]

withDefaultButton :: (MonadIO m, Fresh s m, Rezed s m)
                  => Atlas -> String -> (Slot Button -> m a) -> m a
withDefaultButton atlas str = withButton atlas str buttonPainter
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

textInputForegroundPainter :: MonadIO m
                     =>  Painter (TextInputForegroundData, TextInputState) m
textInputForegroundPainter = Painter $ \(TextInputForegroundData{..}, st) -> do
  let color = textColorForTextInputState st
  return [TexturePainting $ do
       move $ V2 0 $ glyphHeight $ atlasGlyphSize txtnDataAtlas
       freetypePicture txtnDataAtlas color txtnDataString
    ]

textInputBackgroundPainter :: MonadIO m
                           => Painter (TextInputBackgroundData, TextInputState) m
textInputBackgroundPainter = Painter $ \(TextInputBackgroundData{..}, st) -> do
  let textcolor = textColorForTextInputState st
      tsz       = txtnDataSize
  let size@(V2 tw th) = tsz
      bar = setGeometry $ fan $
        mapVertices (,textcolor `withAlpha` 0.5) $
          rectangle (V2 tw padding) (V2 (tw + 1.5) (th + padding))
      hasLeader = st == TextInputStateEditing
      padding = 4
      inc = 1.5 * padding
      bgcolor = bgColorForTextInputState st
      lncolor = lnColorForTextInputState st
  return
    [ColorPainting $ do
      embed $ do
        setStroke [StrokeWidth 3, StrokeFeather 1]
        setGeometry $ do
          fan $ mapVertices (,bgcolor) $ rectangle 0 (size + V2 inc inc)
          line $ do
           to (0, lncolor)
           to (V2 (tw + inc) 0, lncolor)
           to (V2 (tw + inc) (th + inc), lncolor)
           to (V2 0 (th + inc), lncolor)
           to (0, lncolor)
      embed $ do
        move (V2 0 th + V2 0 padding)
        when hasLeader bar
    ]

withDefaultTextInput :: (MonadIO m, Rezed s m, Fresh s m)
                     => Atlas -> String -> (Slot (TextInput m) -> m b) -> m b
withDefaultTextInput atlas str =
  withTextInput atlas str textInputForegroundPainter textInputBackgroundPainter
