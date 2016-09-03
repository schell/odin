{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Text.Internal
  ( withText
  , renderText
  , sizeOfText
  ) where

import Gelatin
import Gelatin.FreeType2
import Gelatin.SDL2
import Control.Lens
import Linear
import Odin.Core

data Text = Text
  { txtUid  :: Int
  , txtSize :: V2 Float
  , txtRndr :: GLRenderer
  }
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
allocText :: (MonadIO m, Fresh s m, Rezed s m)
          => Atlas -> V4 Float -> String -> m (Slot Text)
allocText atlas color str = do
  (sz,dat) <- runPictureT $ do
    freetypePicture atlas color str
    pictureSize
  rz  <- use rez
  txt <- Text <$> fresh <*> pure sz <*> (io $ compileTexturePictureData rz dat)
  allocSlot txt

freeText :: (MonadIO m) => Slot Text -> m ()
freeText s = fromSlot s (fst . txtRndr) >>= io

withText :: (MonadIO m, Fresh s m, Rezed s m)
         => Atlas -> V4 Float -> String -> (Slot Text -> m b) -> m b
withText atlas color str f = do
  txt <- allocText atlas color str
  a <- f txt
  freeText txt
  return a

renderText :: MonadIO m => Slot Text -> [RenderTransform] -> m ()
renderText s rs = do
  txt@Text{..} <- readSlot s
  let t  = rendersToPictureTransform rs
  io $ snd txtRndr t

sizeOfText :: MonadIO m => Slot Text -> m (V2 Float)
sizeOfText = flip fromSlot txtSize
