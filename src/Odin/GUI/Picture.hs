{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI.Picture where

import Odin.Core
import Odin.GUI.Common
import Gelatin.GL

allocPicture :: (Monoid (PictureData t s r v), MonadIO m, Rezed st m, Resources st m)
             => (Rez -> PictureData t s r v -> IO GLRenderer)
             -> PictureT t s r v m a -> m (a, Slot GUIRenderer)
allocPicture compile pic = do
  (a, dat) <- runPictureT pic
  rz       <- use rez
  glr      <- io $ compile rz dat
  s        <- allocSlot glr
  registerFree (fromSlotM s fst)
  return (a, s)

allocColorPicture :: (MonadIO m, Rezed s m, Resources s m)
                  => ColorPictureT m a -> m (a, Slot GUIRenderer)
allocColorPicture = allocPicture compileColorPictureData

allocTexturePicture :: (MonadIO m, Rezed s m, Resources s m)
                  => TexturePictureT m a -> m (a, Slot GUIRenderer)
allocTexturePicture = allocPicture compileTexturePictureData

reallocPicture :: (Monoid (PictureData t s r v), MonadIO m, Rezed st m)
               => (Rez -> PictureData t s r v -> IO GLRenderer)
               -> Slot GUIRenderer -> PictureT t s r v m a -> m a
reallocPicture compile s pic = do
  (a, dat) <- runPictureT pic
  rz       <- use rez
  glr      <- io $ compile rz dat
  old      <- unslot s
  s `is` glr
  io $ fst old
  return a

reallocTexturePicture :: (MonadIO m, Rezed s m)
                      => Slot GUIRenderer -> TexturePictureT m a -> m a
reallocTexturePicture = reallocPicture compileTexturePictureData

reallocColorPicture :: (MonadIO m, Rezed s m)
                      => Slot GUIRenderer -> ColorPictureT m a -> m a
reallocColorPicture = reallocPicture compileColorPictureData

renderPicture :: MonadIO m => Slot GUIRenderer -> [RenderTransform] -> m ()
renderPicture s rs = fromSlotM s $ \(_,r) -> io $ r rs

freePicture :: MonadIO m => Slot GUIRenderer -> m ()
freePicture s = fromSlotM s $ \(c,_) -> io c
