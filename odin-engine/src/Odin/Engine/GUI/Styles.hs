{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Odin.Engine.GUI.Styles
  ( buttonPainter
  , iconButtonPainter
  , textInputPainter
  , slotDefaultText
  ) where

import           Gelatin
import           Gelatin.FreeType2
import           Gelatin.GL (compilePicture)

import Odin.Engine
import Odin.Engine.Slots
import Odin.Engine.GUI.Button
import Odin.Engine.GUI.TextInput
import Odin.Engine.GUI.Text.Internal
import Control.Monad (when)
import Data.Char.FontAwesome

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

buttonPainter
  :: ( Member IO r
     , Member (Reader DefaultFont) r
     , ReadsRenderers r
     , AltersFontMap r
     )
  => Painter (ButtonData, ButtonState) r
buttonPainter = Painter $ \(ButtonData{..}, st) -> do
  DefaultFont font <- readDefaultFontDescriptor
  loadAtlas font asciiChars >>= \case
    Nothing    -> do
      io $ putStrLn "ERROR PAINTING BUTTON!"
      return mempty
    Just atlas0 -> do
      v2v2 <- v2v2Backend
      ((textc,textr), V2 tw th, atlas) <- io $
        freetypeRenderer2 v2v2 atlas0 (textColorForButtonState st) btnDataStr
      saveAtlas atlas

      let pad  = V2 4 4
          sz = V2 tw th + 2*pad
          shxy = V2 4 4
          bgxy = bgOffsetForButtonState st
          gh = glyphHeight $ atlasGlyphSize atlas
          bb = listToBox [shxy, shxy+sz, bgxy, bgxy+sz]

      -- drop shadow and background
      v2v4 <- v2v4Backend
      (_,(bgc,bgr)) <- io $ compilePicture v2v4 $ setGeometry $ do
        fan $ mapVertices (,V4 0 0 0 0.4) $
          rectangle shxy (shxy + sz)
        fan $ mapVertices (,bgColorForButtonState st) $
          rectangle bgxy (bgxy + sz)

      let t = moveV2 $ V2 4 gh + bgxy
          r rs = do bgr rs
                    textr $ t:rs
      return $ Painting bb (bgc >> textc, r)

iconButtonPainter
  :: (Member IO r, Member (Reader IconFont) r, ReadsRenderers r, AltersFontMap r)
  => Painter (ButtonData, ButtonState) r
iconButtonPainter = Painter $ \(ButtonData{..}, st) -> do
  IconFont font <- readIconFontDescriptor
  loadAtlas font allFontAwesomeChars >>= \case
    Nothing    -> do
      io $ putStrLn "ERROR PAINTING BUTTON! Could not load the font atlas"
      return mempty
    Just atlas0 -> do
      v2v2 <- v2v2Backend
      ((textfgc,textfgr), V2 w _, atlas1) <- io $
        freetypeRenderer2 v2v2 atlas0 (bgColorForButtonState st) btnDataStr
      ((textbgc,textbgr), V2 _ _, atlas)  <- io $
        freetypeRenderer2 v2v2 atlas1 (V4 0 0 0 0.4) btnDataStr
      saveAtlas atlas

      let V2 fgx fgy = bgOffsetForButtonState st
          V2 bgx bgy = bgOffsetForButtonState ButtonStateDown
          r rs = do textbgr $ rs ++ [move bgx $ 16 + bgy]
                    textfgr $ rs ++ [move fgx $ 16 + fgy]
          c = textfgc >> textbgc
      return $ Painting (0, V2 (w+2) 18) (c,r)
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

textInputPainter
  :: ( Member IO r
     , Member (Reader DefaultFont) r
     , ReadsRenderers r
     , AltersFontMap r
     )
  =>  Painter (TextInputData, TextInputState) r
textInputPainter = Painter $ \(TextInputData{..}, st) -> do
  DefaultFont font <- readDefaultFontDescriptor
  loadAtlas font asciiChars >>= \case
    Nothing -> do
      io $ putStrLn "ERROR PAINTING TEXTINPUT!"
      return mempty
    Just atlas0 -> do
      let color = textColorForTextInputState st
      v2v2 <- v2v2Backend
      ((textc,textr), size@(V2 tw th), atlas) <- io $
        freetypeRenderer2 v2v2 atlas0 color txtnDataStr
      saveAtlas atlas

      let hasLeader = st == TextInputStateEditing
          padding   = 4
          inc       = 1.5 * padding
          bgcolor   = bgColorForTextInputState st
          lncolor   = lnColorForTextInputState st

      v2v4     <- v2v4Backend
      (bb,(bgc,bgr)) <- io $ compilePicture v2v4 $ do
        setStroke [StrokeWidth 3, StrokeFeather 1]
        setGeometry $ do
          fan $ mapVertices (,bgcolor) $ rectangle 0 (size + V2 inc inc)
          line $ do
            to (0, lncolor)
            to (V2 (tw + inc) 0, lncolor)
            to (V2 (tw + inc) (th + inc), lncolor)
            to (V2 0 (th + inc), lncolor)
            to (0, lncolor)
          when hasLeader $ fan $ mapVertices (,color `withAlpha` 0.5) $
            rectangle (V2 tw padding) (V2 (tw + 1.5) (th + padding))
        pictureBounds2 fst

      let t = move 0 $ glyphHeight $ atlasGlyphSize atlas
          r rs = do bgr rs
                    textr $ t:rs
      return $ Painting bb (bgc >> textc, r)
--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------
slotDefaultText
  :: ( Member IO r
     , Member (Reader DefaultFont) r
     , ReadsRenderers r
     , AltersFontMap r
     , Member Allocates r
     )
  => V4 Float
  -> String
  -> Eff r (Slot Text)
slotDefaultText color str = do
  DefaultFont font <- readDefaultFontDescriptor
  slotText font color str
