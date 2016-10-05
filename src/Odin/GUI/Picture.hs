{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI.Picture where

import Odin.Core
import Odin.GUI.Common
import Gelatin.GL

slotPicture :: (MonadIO m, Resources st m)
            => OdinBackend2 v
            -> PictureT GLuint v m a -> m (a, Slot GUIRenderer)
slotPicture b pic = do
  (a,glr)  <- compilePictureT b pic
  s        <- slot glr
  registerFree (fromSlotM s fst)
  return (a, s)

slotColorPicture :: (MonadIO m, CompileGraphics s m, Resources s m)
                  => ColorPictureT m a -> m (a, Slot GUIRenderer)
slotColorPicture = (v2v4Backend >>=) . flip slotPicture

slotTexturePicture :: (MonadIO m, CompileGraphics s m, Resources s m)
                   => TexturePictureT m a -> m (a, Slot GUIRenderer)
slotTexturePicture = (v2v2Backend >>=) . flip slotPicture

reslotPicture :: ( MonadIO m, CompileGraphics st m)
              => Slot GUIRenderer
              -> OdinBackend2 v -> PictureT GLuint v m a -> m a
reslotPicture s b pic = do
  (a, glr) <- compilePictureT b pic
  old      <- unslot s
  s `is` glr
  io $ fst old
  return a

reslotTexturePicture :: (MonadIO m, CompileGraphics s m)
                      => Slot GUIRenderer -> TexturePictureT m a -> m a
reslotTexturePicture s = (v2v2Backend >>=) . flip (reslotPicture s)

reslotColorPicture :: (MonadIO m, CompileGraphics s m)
                   => Slot GUIRenderer -> ColorPictureT m a -> m a
reslotColorPicture s = (v2v4Backend >>=) . flip (reslotPicture s)

renderPicture :: MonadIO m => Slot GUIRenderer -> [RenderTransform2] -> m ()
renderPicture s rs = fromSlotM s $ \(_,r) -> io $ r rs

freePicture :: MonadIO m => Slot GUIRenderer -> m ()
freePicture s = fromSlotM s $ \(c,_) -> io c
