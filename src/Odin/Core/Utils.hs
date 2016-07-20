module Odin.Core.Utils where

import Gelatin.GL

compilePic :: Rez -> Picture GLuint () -> IO GLRenderer
compilePic rez pic = fst <$> compilePictureRenderer rez mempty pic
