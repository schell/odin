{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Styles
  ( buttonPainter
  , textInputPainter
  , withDefaultButton
  , withDefaultTextInput
  , withDefaultText
  ) where

import           Gelatin.SDL2 hiding (move, scale, rotate)
import qualified Gelatin as G
import           Gelatin.FreeType2
import           Data.Maybe (fromMaybe)
import           Control.Monad (when)
import           Control.Lens hiding (to)

import Odin.Core
import Odin.GUI.Button
import Odin.GUI.TextInput
import Odin.GUI.Text
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

buttonPainter :: (MonadIO m, Rezed s m, Fonts s m)
              => Painter (ButtonData, ButtonState) m
buttonPainter = Painter $ \(ButtonData{..}, st) -> do
  comicFont <- getFontPath "KMKDSP__.ttf"
  loadAtlas (fontDescriptor comicFont 16 ) asciiChars >>= \case
    Nothing    -> do
      io $ putStrLn "ERROR PAINTING BUTTON!"
      return mempty
    Just atlas0 -> do
      rz <- use rez
      (textr, V2 tw th, atlas) <- freetypeGLRenderer rz atlas0
                                                     (textColorForButtonState st)
                                                     btnDataStr
      saveAtlas atlas

      let pad  = V2 4 4
          sz = V2 tw gh + 2*pad
          shxy = V2 4 4
          bgxy = bgOffsetForButtonState st
          gh = glyphHeight $ atlasGlyphSize atlas

      -- drop shadow and background
      (bb,dat) <- runPictureT $ do
        embed $ setGeometry $ fan $
          mapVertices (,V4 0 0 0 0.4) $ rectangle shxy (shxy + sz)
        embed $ setGeometry $ fan $
          mapVertices (,bgColorForButtonState st) $ rectangle bgxy (bgxy + sz)
        pictureBounds
      bgr <- liftIO $ compileColorPictureData rz dat

      let t = mempty{ptfrmMV = affineToModelview $ Translate $ (V2 4 gh) + bgxy}
      return $ Painting (bb, bgr `mappend` transformRenderer t textr)

withDefaultButton :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m)
                  => String -> (Slot Button -> m a) -> m a
withDefaultButton = withButton buttonPainter
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

textInputPainter :: (MonadIO m, Rezed s m, Fonts s m)
                 =>  Painter (TextInputData, TextInputState) m
textInputPainter = Painter $ \(TextInputData{..}, st) -> do
  comicFont <- getFontPath "KMKDSP__.ttf"
  loadAtlas (fontDescriptor comicFont 16) asciiChars >>= \case
    Nothing -> do
      io $ putStrLn "ERROR PAINTING TEXTINPUT!"
      return mempty
    Just atlas0 -> do
      let color = textColorForTextInputState st
      rz <- use rez
      (textr, size@(V2 tw th), atlas) <- freetypeGLRenderer rz atlas0 color
                                                            txtnDataStr
      saveAtlas atlas

      let hasLeader = st == TextInputStateEditing
          padding = 4
          inc = 1.5 * padding
          bgcolor = bgColorForTextInputState st
          lncolor = lnColorForTextInputState st

      (bb,dat) <- runPictureT $ do
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
        when hasLeader $ embed $ do
          G.move (V2 0 th + V2 0 padding)
          setGeometry $ fan $
            mapVertices (,color `withAlpha` 0.5) $
              rectangle (V2 tw padding) (V2 (tw + 1.5) (th + padding))
        pictureBounds

      bgr <- io $ compileColorPictureData rz dat

      let t = mempty{ptfrmMV = affineToModelview $ Translate $
                  V2 0 $ glyphHeight $ atlasGlyphSize atlas
               }
      return $ Painting (bb, bgr `mappend` transformRenderer t textr)

withDefaultTextInput :: (MonadIO m, Rezed s m, Fresh s m, Fonts s m)
                     => String -> (Slot (TextInput m) -> m b) -> m b
withDefaultTextInput = withTextInput textInputPainter
--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------
withDefaultText :: (MonadIO m, Rezed s m, Fresh s m, Fonts s m)
                => V4 Float -> String -> (Slot Text -> m b) -> m b
withDefaultText color str f = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  withText (fontDescriptor comicFont 16) color str f
