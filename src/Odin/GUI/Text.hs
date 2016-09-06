{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Odin.GUI.Text
  ( Text
  , withText
  , renderText
  , sizeOfText
  ) where

import Gelatin.FreeType2
import Gelatin.SDL2
import Control.Lens
import Odin.Core

data Text = Text
  { txtUid  :: Int
  , txtSize :: V2 Float
  , txtRndr :: GLRenderer
  }
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
allocText :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m)
          => FontDescriptor -> V4 Float -> String -> m (Slot Text)
allocText desc color str = loadAtlas desc asciiChars >>= \case
  Nothing -> do
    io $ putStrLn "ERROR ALLOCING TEXT!"
    allocSlot $ Text 0 0 mempty
  Just atlas0 -> do
    rz  <- use rez
    (r,sz,atlas) <- freetypeGLRenderer rz atlas0 color str
    saveAtlas atlas
    txt <- Text <$> fresh <*> pure sz <*> pure r
    allocSlot txt

freeText :: (MonadIO m) => Slot Text -> m ()
freeText s = fromSlot s (fst . txtRndr) >>= io

withText :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m)
         => FontDescriptor -> V4 Float -> String -> (Slot Text -> m b) -> m b
withText desc color str f = do
  txt <- allocText desc color str
  a <- f txt
  freeText txt
  return a

renderText :: MonadIO m => Slot Text -> [RenderTransform] -> m ()
renderText s rs = do
  Text{..} <- readSlot s
  let t  = rendersToPictureTransform rs
  io $ snd txtRndr t

sizeOfText :: MonadIO m => Slot Text -> m (V2 Float)
sizeOfText = flip fromSlot txtSize
