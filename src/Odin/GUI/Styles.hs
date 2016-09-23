{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Styles
  ( getDefaultFontDescriptor
  , buttonPainter
  , iconButtonPainter
  , textInputPainter
  , slotDefaultText
  ) where

import           Gelatin hiding (move)
import qualified Gelatin as G
import           Gelatin.FreeType2
import           Gelatin.GL (transformRenderer, compileColorPictureData)

import Odin.Core
import Odin.GUI.Button
import Odin.GUI.TextInput
import Odin.GUI.Text.Internal
import Odin.GUI.StatusBar
import Control.Monad (when)
import Data.Char.FontAwesome

getDefaultFontDescriptor :: MonadIO m => m FontDescriptor
getDefaultFontDescriptor = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  return $ fontDescriptor comicFont 16
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
      ((textc,textr), V2 tw th, atlas) <- freetypeGLRenderer rz atlas0
                                                    (textColorForButtonState st)
                                                    btnDataStr
      saveAtlas atlas

      let pad  = V2 4 4
          sz = V2 tw th + 2*pad
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
      (bgc,bgr) <- io $ compileColorPictureData rz dat

      let t = moveV2 $ (V2 4 gh) + bgxy
          r rs = do bgr rs
                    textr $ t:rs
      return $ Painting (bb, (bgc >> textc, r))

iconButtonPainter :: (MonadIO m, Rezed s m, Fonts s m)
                  => Int -> Painter (ButtonData, ButtonState) m
iconButtonPainter px = Painter $ \(ButtonData{..}, st) -> do
  iconFont <- getFontPath "FontAwesome.otf"
  loadAtlas (fontDescriptor iconFont px ) allFontAwesomeChars >>= \case
    Nothing    -> do
      io $ putStrLn "ERROR PAINTING BUTTON!"
      return mempty
    Just atlas0 -> do
      rz <- use rez
      ((textfgc,textfgr), V2 w _, atlas1) <- freetypeGLRenderer rz atlas0
                                                    (bgColorForButtonState st)
                                                    btnDataStr
      ((textbgc,textbgr), V2 _ _, atlas)  <- freetypeGLRenderer rz atlas1
                                                     (V4 0 0 0 0.4)
                                                     btnDataStr
      saveAtlas atlas

      let p = bgOffsetForButtonState st
          v = V2 0 $ fromIntegral px
          r rs = do textbgr $ rs ++ [move 2 $ fromIntegral px + 2]
                    textfgr $ rs ++ [moveV2 $ p + v]
          c = textfgc >> textbgc
      return $ Painting ((0, V2 (w+2) (fromIntegral px+2)), (c,r))
--------------------------------------------------------------------------------
-- TextInput Styling
--------------------------------------------------------------------------------
textColorForTextInputState :: TextInputState -> V4 Float
textColorForTextInputState st = if st == TextInputStateEditing then canary else white

bgColorForTextInputState :: TextInputState -> V4 Float
bgColorForTextInputState TextInputStateDown = V4 0.3 0.3 0.3 1
bgColorForTextInputState TextInputStateEditing = V4 0.2 0.2 0.2 1
bgColorForTextInputState _ = V4 0 0 0 1

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
      ((textc,textr), size@(V2 tw th), atlas) <- freetypeGLRenderer rz atlas0
                                                                    color
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
        when hasLeader $ embed $ setGeometry $ fan $
            mapVertices (,color `withAlpha` 0.5) $
              rectangle (V2 tw padding) (V2 (tw + 1.5) (th + padding))
        pictureBounds

      (bgc,bgr) <- io $ compileColorPictureData rz dat

      let t = move 0 $ glyphHeight $ atlasGlyphSize atlas
          r rs = do bgr rs
                    textr $ t:rs
      return $ Painting (bb, (bgc >> textc, r))
--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------
slotDefaultText :: (MonadIO m, Rezed s m, Resources s m, Fonts s m)
                 => V4 Float -> String -> m (Slot Text)
slotDefaultText color str = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  slotText (fontDescriptor comicFont 16) color str
