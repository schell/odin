{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Odin.GUI.Text.Internal where

import Gelatin.FreeType2
import Gelatin.SDL2
import Control.Lens
import Odin.Core
import Odin.GUI.Common
--------------------------------------------------------------------------------
-- Grapical Text
--------------------------------------------------------------------------------
data Text = Text
  { txtSize :: V2 Float
  , txtRndr :: Renderer2
  }

compileText :: (MonadIO m, CompileGraphics s m, Fonts s m)
            => FontDescriptor -> V4 Float -> String -> m Text
compileText desc color str = loadAtlas desc asciiChars >>= \case
  Nothing -> do
    io $ putStrLn "ERROR ALLOCING TEXT!"
    return $ Text 0 mempty
  Just atlas0 -> do
    b  <- v2v2Backend
    (r,sz,atlas) <- freetypeRenderer2 b atlas0 color str
    saveAtlas atlas
    return $ Text sz r

-- | Slots a graphical text renderer.
slotText :: (MonadIO m, CompileGraphics s m, Fonts s m, Resources s m)
          => FontDescriptor -> V4 Float -> String -> m (Slot Text)
slotText desc color str = do
  txt <- compileText desc color str
  s   <- slot txt
  registerFree $ freeText s
  return s

-- | Reslots a graphical text renderer, allowing you to change the text, font
-- or color.
reslotText :: (MonadIO m, CompileGraphics s m, Fonts s m)
            => Slot Text -> FontDescriptor -> V4 Float -> String -> m ()
reslotText s desc color str = compileText desc color str >>= reslot s

freeText :: (MonadIO m) => Slot Text -> m ()
freeText s = fromSlot s (fst . txtRndr) >>= io

-- | Renders a slotted text renderer.
renderText :: MonadIO m => Slot Text -> [RenderTransform2] -> m ()
renderText s rs = do
  Text{..} <- unslot s
  io $ snd txtRndr rs

--- | Retrieves the size of the slotted text.
sizeOfText :: MonadIO m => Slot Text -> m (V2 Float)
sizeOfText = flip fromSlot txtSize
