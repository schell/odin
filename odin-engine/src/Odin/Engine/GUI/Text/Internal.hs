{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Odin.Engine.GUI.Text.Internal where

import Gelatin.FreeType2
import Gelatin.SDL2
--------------------------------------------------------------------------------
import Odin.Engine
import Odin.Engine.Slots
--------------------------------------------------------------------------------
-- Grapical Text
--------------------------------------------------------------------------------
data Text = Text { txtSize :: V2 Float
                 , txtRndr :: Renderer2
                 }

compileText
  :: (Member IO r, ReadsRenderers r, AltersFontMap r, Member IO r)
  => FontDescriptor
  -> V4 Float
  -> String
  -> Eff r Text
compileText desc color str = loadAtlas desc asciiChars >>= \case
  Nothing -> do
    io $ putStrLn "ERROR ALLOCING TEXT!"
    return $ Text 0 mempty
  Just atlas0 -> do
    b  <- v2v2Backend
    (r, sz, atlas) <- io $ freetypeRenderer2 b atlas0 color str
    saveAtlas atlas
    return $ Text sz r

-- | Slots a graphical text renderer.
slotText
  :: (Member IO r, ReadsRenderers r, AltersFontMap r, Member Allocates r)
  => FontDescriptor
  -> V4 Float
  -> String
  -> Eff r (Slot Text)
slotText desc color str = compileText desc color str >>= (`slot` freeText)

-- | Reslots a graphical text renderer, allowing you to change the text, font
-- or color.
reslotText
  :: (Member IO r, ReadsRenderers r, AltersFontMap r)
  => Slot Text -> FontDescriptor -> V4 Float -> String -> Eff r ()
reslotText s desc color str = compileText desc color str >>= reslot s

freeText :: Text -> IO ()
freeText = fst . txtRndr

-- | Renders a slotted text renderer.
renderText
  :: Member IO r => Slot Text -> [RenderTransform2] -> Eff r ()
renderText s rs = do
  Text{..} <- unslot s
  io $ snd txtRndr rs

--- | Retrieves the size of the slotted text.
sizeOfText :: Member IO r => Slot Text -> Eff r (V2 Float)
sizeOfText = flip fromSlot txtSize
