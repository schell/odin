{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Layer
  ( Layer(..)
  , allocLayer
  , reallocLayer
  , renderLayer
  ) where

import           Gelatin.SDL2 hiding (move, scale, rotate)
import           Odin.Core
import           Odin.GUI.Common
import           Odin.GUI.Picture
import           Foreign.Marshal hiding (void)
--------------------------------------------------------------------------------
-- Layer
--------------------------------------------------------------------------------
-- | A Layer is an offscreen buffer that can be rendered to, and then
-- transformed and rendered separately. It is used for GUI elements like Pane
-- and Panel.
data Layer = Layer { layerFramebuffer     :: GLuint
                   , layerTexture         :: GLuint
                   , layerSize            :: V2 Int
                   , layerBackgroundColor :: V4 Float
                   , layerPicture         :: Slot GLRenderer
                   }

allocLayerFBTex :: MonadIO m => V2 Int -> m (GLuint, GLuint)
allocLayerFBTex (V2 w h) = do
  [fb] <- io $ allocaArray 1 $ \ptr -> do
    glGenFramebuffers 1 ptr
    peekArray 1 ptr
  glBindFramebuffer GL_FRAMEBUFFER fb
  tex <- io $ allocAndActivateTex GL_TEXTURE0
  io $ initializeTexImage2D (fromIntegral $ 2*w) (fromIntegral $ 2*h)
  glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 tex 0
  io $ withArray [GL_COLOR_ATTACHMENT0] $ glDrawBuffers 1
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  unless (status == GL_FRAMEBUFFER_COMPLETE) $
    io $ putStrLn "allocLayer: could not complete the framebuffer!"
  glBindFramebuffer GL_FRAMEBUFFER 0
  return (fb, tex)

layerPic :: Monad m => V2 Int -> GLuint -> TexturePictureT m ()
layerPic size tex = do
  let (V2 wf hf) = fromIntegral <$> size
  setTextures [tex]
  setGeometry $ fan $ do
    to (0       , V2 0  0)
    to (V2 wf  0, V2 1  0)
    to (V2 wf hf, V2 1  (-1))
    to (V2 0  hf, V2 0  (-1))

-- | Allocs an offscreen buffer of `size`. This is like creating an
-- entirely new window within the current context.
allocLayer :: GUI s m => V2 Int -> V4 Float -> m (Slot Layer)
allocLayer size color = do
  -- alloc our framebuffer and texture to fit the given size
  (fb, tex) <- allocLayerFBTex size
  -- alloc our quad-painting picture
  (_, img)  <- allocTexturePicture $ layerPic size tex
  -- register some automatic dealloc'ing
  s <- slot $ Layer fb tex size color img
  registerFree $ freeLayer s
  return s

-- | Frees any GPU resources allocated by `allocLayer`.
freeLayer :: MonadIO m => Slot Layer -> m ()
freeLayer s = do
  Layer{..} <- unslot s
  io $ do
    with layerTexture $ glDeleteTextures 1
    with layerFramebuffer  $ glDeleteFramebuffers 1

-- | Reallocs a layer.
reallocLayer :: GUI s m => Slot Layer -> V2 Int -> m ()
reallocLayer s size = do
  l@Layer{..} <- unslot s

  io $ do
    with layerTexture $ glDeleteTextures 1
    with layerFramebuffer  $ glDeleteFramebuffers 1
  (fb,tex) <- allocLayerFBTex size

  reallocTexturePicture layerPicture $ layerPic size tex
  swapSlot s l{layerTexture=tex
              ,layerFramebuffer=fb
              ,layerSize=size
              }

-- | Render something into the offscreen frame and then display that frame at
-- the given transform.
renderLayer :: GUI s m => Slot Layer -> [RenderTransform] -> m a -> m a
renderLayer s rs f = do
  Layer{..} <- unslot s
  let V4 r g b a = layerBackgroundColor
  -- target the layerrender's framebuffer so all rendering goes there
  glBindFramebuffer GL_FRAMEBUFFER layerFramebuffer
  glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT
  -- flip and translate the lower left y coordinate so we draw into our attached
  -- texture with the correct orientation
  fbsz@(V2 _ fhf) <- getFramebufferSize
  let V2 _ wh  = fromIntegral <$> layerSize
      V2 fw fh = floor <$> fbsz
  glViewport 0 (floor $ -fhf + 2 * wh) fw fh
  -- do the layer rendering
  a <- f
  -- fix the viewport for later renderings
  glViewport 0 0 fw fh
  -- target the default framebuffer
  glBindFramebuffer GL_FRAMEBUFFER 0
  -- render our texture quad
  renderPicture layerPicture rs
  return a
