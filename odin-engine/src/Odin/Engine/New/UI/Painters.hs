{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.New.UI.Painters
  ( getButtonPainter
  , getBlankButtonPainter
  , getIconButtonPainter
  , getTextInputPainter
  ) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Gelatin
import           Gelatin.FreeType2
import           Gelatin.GL                 (compilePicture, V2V4)

import           Control.Monad              (when)
import           Data.Char.FontAwesome
import           Odin.Engine.New
import           Odin.Engine.New.UI.Painting
import           Odin.Engine.New.UI.Configs

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
bgColorForButtonState _               = bgColorForButtonState ButtonStateUp


bgOffsetForButtonState :: ButtonState -> V2 Float
bgOffsetForButtonState ButtonStateUp   = V2 0 0
bgOffsetForButtonState ButtonStateOver = V2 0 0
bgOffsetForButtonState ButtonStateDown = V2 2 2
bgOffsetForButtonState _               = bgOffsetForButtonState ButtonStateUp


getButtonPainterWithFont
  :: Odin r t m
  => FontDescriptor
  -> m (Painter (ButtonData String) IO)
getButtonPainterWithFont font = do
  V2V2Renderer v2v2 <- getV2V2
  tvFontMap         <- getTVarFontMap
  blankPntr         <- getBlankButtonPainter
  return $ Painter $ \(ButtonData txt st) ->
    loadAtlasInto tvFontMap font txt >>= \case
      Nothing    -> do
        putStrLn "ERROR PAINTING BUTTON!"
        return mempty
      Just atlas0 -> do
        ((textc, textr), V2 tw th, atlas) <-
          freetypeRenderer2 v2v2 atlas0 (textColorForButtonState st) txt
        saveAtlasInto tvFontMap atlas

        let pad  = V2 4 4
            sz   = V2 tw th + 2*pad
            bgxy = bgOffsetForButtonState st
            gh   = glyphHeight $ atlasGlyphSize atlas

        Painting bb (bgc, bgr) <-
          runPainter blankPntr (ButtonData sz st)

        let t = moveV2 $ V2 4 gh + bgxy
            r rs = do bgr rs
                      textr $ t:rs
        return $ Painting bb (bgc >> textc, r)


getBlankButtonPainter
  :: Odin r t m
  => m (Painter (ButtonData (V2 Float)) IO)
getBlankButtonPainter = do
  V2V4Renderer v2v4 <- getV2V4
  return $ Painter $ \(ButtonData sz st) -> do
    let shxy = V2 4 4
        bgxy = bgOffsetForButtonState st

    -- drop shadow and background
    (_, r) <- compilePicture v2v4 $ setGeometry $ do
      fan $ mapVertices (,V4 0 0 0 0.4) $
        rectangle shxy (shxy + sz)
      fan $ mapVertices (,bgColorForButtonState st) $
        rectangle bgxy (bgxy + sz)

    let bb = listToBox [0, shxy, shxy+sz, bgxy, bgxy+sz]
    return $ Painting bb r


getButtonPainter :: Odin r t m => m (Painter (ButtonData String) IO)
getButtonPainter = do
  DefaultFont font <- getDefaultFont
  tvFontMap        <- getTVarFontMap
  _ <- loadAtlasInto tvFontMap font asciiChars
  getButtonPainterWithFont font


getIconButtonPainter :: Odin r t m => m (Painter (ButtonData Char) IO)
getIconButtonPainter = do
  IconFont font <- getIconFont
  tvFontMap     <- getTVarFontMap
  _ <- loadAtlasInto tvFontMap font allFontAwesomeChars
  pntr <- getButtonPainterWithFont font
  return $ Painter $ \(ButtonData c st) -> runPainter pntr (ButtonData [c] st)


{-getIconButtonPainter
  :: Odin r t m
  => m (Painter ButtonData IO)-}
-- getIconButtonPainter = do
--   V2V2Renderer v2v2 <- getV2V2
--   IconFont font     <- getIconFont
--   tvFontMap         <- getTVarFontMap
--   return $ Painter $ \(ButtonData txt st) ->
--     loadAtlasInto tvFontMap font allFontAwesomeChars >>= \case
--       Nothing    -> do
--         putStrLn "ERROR PAINTING BUTTON! Could not load the font atlas"
--         return mempty
--       Just atlas0 -> do
--         ((textfgc,textfgr), V2 w _, atlas1) <-
--           freetypeRenderer2 v2v2 atlas0 (bgColorForButtonState st) txt
--         ((textbgc,textbgr), V2 _ _, atlas)  <-
--           freetypeRenderer2 v2v2 atlas1 (V4 0 0 0 0.4) txt
--         saveAtlasInto tvFontMap atlas

--         let V2 fgx fgy = bgOffsetForButtonState st
--             V2 bgx bgy = bgOffsetForButtonState ButtonStateDown
--             r rs = do textbgr $ rs ++ [move bgx $ 32 + bgy]
--                       textfgr $ rs ++ [move fgx $ 32 + fgy]
--             c = textfgc >> textbgc
--         return $ Painting (0, V2 (w+2) 34) (c,r)

---------------------------------------------------------------------------------
---- TextInput Styling
----------------------------------------------------------------------------------
textColorForTextInputState :: TextInputState -> V4 Float
textColorForTextInputState st = if st == TextInputStateEditing then canary else white

placeholderTextColor :: V4 Float
placeholderTextColor = grey

bgColorForTextInputState :: TextInputState -> V4 Float
bgColorForTextInputState TextInputStateDown    = V4 0.3 0.3 0.3 1
bgColorForTextInputState TextInputStateEditing = V4 0.2 0.2 0.2 1
bgColorForTextInputState _                     = V4 0 0 0 1

lnColorForTextInputState :: TextInputState -> V4 Float
lnColorForTextInputState TextInputStateUp   = white `withAlpha` 0.4
lnColorForTextInputState TextInputStateOver = white `withAlpha` 0.8
lnColorForTextInputState TextInputStateDown = white
lnColorForTextInputState _                  = white `withAlpha` 0.8

getTextInputPainter
  :: Odin r t m
  => m (Painter TextInputData IO)
getTextInputPainter = do
  V2V2Renderer v2v2 <- getV2V2
  V2V4Renderer v2v4 <- getV2V4
  DefaultFont font  <- getDefaultFont
  tvFontMap         <- getTVarFontMap
  return $ Painter $ \(TextInputData txt placeholder st) ->
    loadAtlasInto tvFontMap font asciiChars >>= \case
      Nothing -> do
        putStrLn "ERROR PAINTING TEXTINPUT!"
        return mempty
      Just atlas0 -> do
        let color = textColorForTextInputState st
        ((textc, textr), V2 ttw tth, atlas1) <-
          freetypeRenderer2 v2v2 atlas0 color txt
        ((ptextc, ptextr), V2 ptw pth, atlas) <-
          freetypeRenderer2 v2v2 atlas1 placeholderTextColor placeholder
        saveAtlasInto tvFontMap atlas

        let size@(V2 tw th) = V2 (max ttw ptw) (max tth pth)
            hasLeader       = st == TextInputStateEditing
            padding         = 4
            inc             = 2 * padding
            bgcolor         = bgColorForTextInputState st
            lncolor         = lnColorForTextInputState st

        (bb,(bgc,bgr)) <- compilePicture v2v4 $ do
          setStroke [StrokeWidth 3, StrokeFeather 1]
          setGeometry $ do
            fan $ mapVertices (,bgcolor) $ rectangle 0 (size + V2 inc inc)
            line $ do
              to (0, lncolor)
              to (V2 (tw + inc) 0, lncolor)
              to (V2 (tw + inc) (th + inc), lncolor)
              to (V2 0 (th + inc), lncolor)
              to (0, lncolor)
            when hasLeader $ fan $ mapVertices (,color `withAlpha` 0.5) $ do
              let lxy = V2 (padding + ttw) padding
                  lwh = V2 1.5 tth
              rectangle lxy (lxy + lwh)
          pictureBounds2 fst

        let t = move padding $ glyphHeight $ atlasGlyphSize atlas
            r rs = do bgr rs
                      if null txt && st /= TextInputStateEditing
                      then ptextr $ t:rs
                      else textr $ t:rs
        return $ Painting bb (bgc >> ptextc >> textc, r)
