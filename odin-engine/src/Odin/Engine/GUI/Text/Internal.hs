{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Odin.Engine.GUI.Text.Internal where

import Gelatin.FreeType2
import Gelatin.SDL2
import Odin.Engine.Eff
import Odin.Engine.GUI.Common
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
slotText
  :: (MonadIO m, CompileGraphics s m, Fonts s m)
  => FontDescriptor -> V4 Float -> String -> AllocatedT os m (Slot os Text)
slotText desc color str = compileText desc color str >>= (`slot` freeText)

-- | Reslots a graphical text renderer, allowing you to change the text, font
-- or color.
reslotText
  :: (MonadIO m, CompileGraphics s m, Fonts s m)
  => Slot os Text -> FontDescriptor -> V4 Float -> String -> AllocatedT os m ()
reslotText s desc color str = compileText desc color str >>= reslot s

freeText :: (MonadIO m) => Text -> m ()
freeText = io . fst . txtRndr

-- | Renders a slotted text renderer.
renderText
  :: MonadIO m => Slot os Text -> [RenderTransform2] -> AllocatedT os m ()
renderText s rs = do
  Text{..} <- unslot s
  io $ snd txtRndr rs

--- | Retrieves the size of the slotted text.
sizeOfText :: MonadIO m => Slot os Text -> AllocatedT os m (V2 Float)
sizeOfText = flip fromSlot txtSize
