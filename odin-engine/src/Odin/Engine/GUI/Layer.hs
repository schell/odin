{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Odin.Engine.GUI.Layer
  ( Layer(..)
  , slotLayer
  , reslotLayer
  , renderLayer
  ) where

import           Control.Monad           (unless)
import           Foreign.Marshal         hiding (void)
import           Gelatin.SDL2            hiding (move, rotate, scale)
--------------------------------------------------------------------------------
import           Odin.Engine
import           Odin.Engine.GUI.Picture
import           Odin.Engine.Slots
--------------------------------------------------------------------------------
-- Layer
-- | TODO: Move a bunch of this stuff into the backend.
--------------------------------------------------------------------------------
-- | A Layer is an offscreen buffer that can be rendered to, and then
-- transformed and rendered separately. It is used for GUI elements like Pane
-- and Panel.
data Layer = Layer { layerFramebuffer     :: GLuint
                   , layerTexture         :: GLuint
                   , layerSize            :: V2 Int
                   , layerBackgroundColor :: V4 Float
                   , layerPicture         :: Slot Renderer2
                   }

allocLayerFBTex :: V2 Int -> IO (GLuint, GLuint)
allocLayerFBTex (V2 w h) = do
  [fb] <- allocaArray 1 $ \ptr -> do
    glGenFramebuffers 1 ptr
    peekArray 1 ptr
  glBindFramebuffer GL_FRAMEBUFFER fb
  tex <- allocAndActivateTex GL_TEXTURE0
  initializeTexImage2D (fromIntegral $ 2*w) (fromIntegral $ 2*h)
  glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 tex 0
  withArray [GL_COLOR_ATTACHMENT0] $ glDrawBuffers 1
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  unless (status == GL_FRAMEBUFFER_COMPLETE) $
    putStrLn "slotLayer: could not complete the framebuffer!"
  glBindFramebuffer GL_FRAMEBUFFER 0
  return (fb, tex)

layerPic :: V2 Int -> GLuint -> TexturePicture ()
layerPic size tex = do
  let (V2 wf hf) = fromIntegral <$> size
  setTextures [tex]
  setGeometry $ fan $ do
    to (0       , V2 0  0)
    to (V2 wf  0, V2 1  0)
    to (V2 wf hf, V2 1  (-1))
    to (V2 0  hf, V2 0  (-1))

-- | Slots an offscreen buffer of `size`. This is like creating an
-- entirely new window within the current context.
slotLayer
  :: (Member IO r, ReadsRenderers r, AltersUI r, Member Allocates r)
  => V2 Int
  -> V4 Float
  -> Eff r (Slot Layer)
slotLayer size color = do
  -- alloc our framebuffer and texture to fit the given size
  (fb, tex) <- io $ allocLayerFBTex size
  -- slot our quad-painting picture
  (_, img)  <- slotTexturePicture $ layerPic size tex
  slot (Layer fb tex size color img) freeLayer

-- | Frees any GPU resources allocated by `allocLayer`.
freeLayer :: Layer -> IO ()
freeLayer Layer{..} = do
  with layerTexture     $ glDeleteTextures 1
  with layerFramebuffer $ glDeleteFramebuffers 1

-- | Reslots a layer.
reslotLayer
  :: (Member IO r, ReadsRenderers r, AltersUI r)
  => Slot Layer
  -> V2 Int
  -> Eff r ()
reslotLayer s size = do
  l@Layer{..} <- unslot s

  io $ do
    with layerTexture $ glDeleteTextures 1
    with layerFramebuffer  $ glDeleteFramebuffers 1
  (fb,tex) <- io $ allocLayerFBTex size

  reslotTexturePicture layerPicture $ layerPic size tex
  reslot s l{layerTexture=tex
            ,layerFramebuffer=fb
            ,layerSize=size
            }

-- | Render something into the offscreen frame and then display that frame at
-- the given transform.
renderLayer
  :: (Member IO r, ReadsRenderers r, AltersUI r)
  => Slot Layer
  -> [RenderTransform2]
  -> Eff r a
  -> Eff r a
renderLayer s rs f = do
  Layer{..} <- unslot s
  let V4 r g b a = layerBackgroundColor
  -- target the layerrender's framebuffer so all rendering goes there
  io $ do
    glBindFramebuffer GL_FRAMEBUFFER layerFramebuffer
    glClearColor r g b a
    glClear GL_COLOR_BUFFER_BIT
  -- flip and translate the lower left y coordinate so we draw into our attached
  -- texture with the correct orientation
  fbsz@(V2 _ fhf) <- getFramebufferSize
  let V2 _ wh  = fromIntegral <$> layerSize
      V2 fw fh = floor <$> fbsz

  io $ glViewport 0 (floor $ -fhf + 2 * wh) fw fh
  -- do the layer rendering
  x <- f
  -- fix the viewport for later renderings
  io $ do
    glViewport 0 0 fw fh
    -- target the default framebuffer
    glBindFramebuffer GL_FRAMEBUFFER 0
  -- render our texture quad
  renderPicture layerPicture rs
  return x
