{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Odin.Engine.GUI.Picture where

import           Control.Monad.IO.Class (MonadIO (..))

import           Odin.Engine
import           Odin.Engine.Slots

slotPicture
  :: (MonadIO m, MonadSafe m)
  => OdinRenderer v
  -> Picture GLuint v a -> m (a, Slot Renderer2)
slotPicture b pic = do
  (a, glr)  <- liftIO $ compilePicture b pic
  s         <- slot glr fst
  return (a, s)

slotColorPicture
  :: (MonadIO m, ReadsRenderers m, MonadSafe m)
  => ColorPicture a
  -> m (a, Slot Renderer2)
slotColorPicture = (v2v4Backend >>=) . flip slotPicture

slotTexturePicture
  :: (MonadIO m, ReadsRenderers m, MonadSafe m)
  => TexturePicture a
  -> m (a, Slot Renderer2)
slotTexturePicture = (v2v2Backend >>=) . flip slotPicture

reslotPicture
  :: (MonadIO m, ReadsRenderers m)
  => Slot Renderer2
  -> OdinRenderer v
  -> Picture GLuint v a
  -> m a
reslotPicture s b pic = do
  freePicture s
  (a, glr) <- liftIO $ compilePicture b pic
  s `is` glr
  return a

reslotTexturePicture
  :: (MonadIO m, ReadsRenderers m)
  => Slot Renderer2 -> TexturePicture a
  -> m a
reslotTexturePicture s = (v2v2Backend >>=) . flip (reslotPicture s)

reslotColorPicture
  :: (MonadIO m, ReadsRenderers m)
  => Slot Renderer2
  -> ColorPicture a -> m a
reslotColorPicture s = (v2v4Backend >>=) . flip (reslotPicture s)

renderPicture
  :: MonadIO m
  => Slot Renderer2
  -> [RenderTransform2]
  -> m ()
renderPicture s rs = fromSlotM s $ \(_,r) -> liftIO $ r rs

freePicture
  :: MonadIO m
  => Slot Renderer2
  -> m ()
freePicture s = fromSlotM s $ \(c,_) -> liftIO c
