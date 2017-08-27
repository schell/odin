{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module Odin.Engine.GUI.Text.Internal where

import           Control.Monad.IO.Class (MonadIO (..))
import           Gelatin.FreeType2
import           Gelatin.SDL2
--------------------------------------------------------------------------------
import           Odin.Engine
import           Odin.Engine.Slots
--------------------------------------------------------------------------------
-- Grapical Text
--------------------------------------------------------------------------------
data Text = Text { txtSize :: V2 Float
                 , txtRndr :: Renderer2
                 }

compileText
  :: (MonadIO m, ReadsRenderers m, Mutate FontMap m, MonadIO m)
  => FontDescriptor
  -> V4 Float
  -> String
  -> m Text
compileText desc color str = loadAtlas desc asciiChars >>= \case
  Nothing -> do
    liftIO $ putStrLn "ERROR ALLOCING TEXT!"
    return $ Text 0 mempty
  Just atlas0 -> do
    b  <- v2v2Backend
    (r, sz, atlas) <- liftIO $ freetypeRenderer2 b atlas0 color str
    saveAtlas atlas
    return $ Text sz r

-- | Slots a graphical text renderer.
slotText
  :: (MonadIO m, ReadsRenderers m, Mutate FontMap m, MonadSafe m)
  => FontDescriptor
  -> V4 Float
  -> String
  -> m (Slot Text)
slotText desc color str = compileText desc color str >>= (`slot` freeText)

-- | Reslots a graphical text renderer, allowing you to change the text, font
-- or color.
reslotText
  :: (MonadIO m, ReadsRenderers m, Mutate FontMap m)
  => Slot Text -> FontDescriptor -> V4 Float -> String -> m ()
reslotText s desc color str = compileText desc color str >>= reslot s

freeText :: Text -> IO ()
freeText = fst . txtRndr

-- | Renders a slotted text renderer.
renderText
  :: MonadIO m => Slot Text -> [RenderTransform2] -> m ()
renderText s rs = do
  Text{..} <- unslot s
  liftIO $ snd txtRndr rs

--- | Retrieves the size of the slotted text.
sizeOfText :: MonadIO m => Slot Text -> m (V2 Float)
sizeOfText = flip fromSlot txtSize
