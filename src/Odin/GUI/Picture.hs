{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Picture where

import Odin.Core.Common
import Gelatin.GL

withPicture :: (Monoid (PictureData t s r v), MonadIO m, Rezed st m)
            => (Rez -> PictureData t s r v -> IO GLRenderer)
            -> PictureT t s r v m a -> ((a, RenderGUI m ()) -> m b)
            -> m b
withPicture compile pic f = do
  (a, dat) <- runPictureT pic
  rz       <- use rez
  r        <- io $ compile rz dat
  b        <- f (a, io . snd r . rendersToPictureTransform)
  io $ fst r
  return b

withColorPicture :: (MonadIO m, Rezed s m)
                 => ColorPictureT m a -> ((a, RenderGUI m ()) -> m b) -> m b
withColorPicture = withPicture compileColorPictureData

withTexturePicture :: (MonadIO m, Rezed s m)
                   => TexturePictureT m a -> ((a, RenderGUI m ()) -> m b) -> m b
withTexturePicture = withPicture compileTexturePictureData
