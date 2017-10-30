{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | A Layer is an offscreen buffer that can be rendered to, and then
-- transformed and rendered separately. It is used for GUI elements like Pane
-- and Panel.
module Odin.Engine.New.UI.Layer
  ( layer
  , LayerCfg (..)
  ) where

import           Control.Arrow              ((&&&))
import           Control.Monad              (forM_, msum, unless, void)
import           Control.Monad.Reader       (local)
import qualified Data.Map                   as M
import           Data.Word                  (Word64)
import           Foreign.Marshal            hiding (void)
import           Gelatin.SDL2               hiding (move, rotate, scale)
import           Reflex.SDL2                hiding (fan)
import           Reflex.SDL2.Internal

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs


--------------------------------------------------------------------------------
data LayerInternal = LayerInternal { liK               :: Word64
                                   , liWidgetsK        :: Word64
                                   , liFramebuffer     :: GLuint
                                   , liTexture         :: GLuint
                                   , liSize            :: V2 Int
                                   , liBackgroundColor :: V4 Float
                                   , liBoundary        :: Shape
                                   , liWidgets         :: [Widget]
                                   , liPictureRenderer :: Renderer2
                                   }


freeLayerFBTex :: LayerInternal -> IO ()
freeLayerFBTex l = do
  with (liTexture l)     $ glDeleteTextures 1
  with (liFramebuffer l) $ glDeleteFramebuffers 1


toWidgets :: LayerInternal -> [Widget]
toWidgets l =
  let (cleanPic, rendPic) = liPictureRenderer l
      pictureWidget = Widget { widgetUid       = liK l
                             , widgetTransform = []
                             , widgetBoundary  = [liBoundary l]
                             , widgetRenderer2 =
                               (cleanPic >> freeLayerFBTex l, rendPic)
                             , widgetCursor    =
                               msum $ reverse $ map widgetCursor $ liWidgets l
                             }
      cleanupSubsWidget = Widget { widgetUid       = liWidgetsK l
                                 , widgetTransform = []
                                 , widgetBoundary  = []
                                 , widgetRenderer2 =
                                   (mapM_ freeWidget $ liWidgets l, const $ return ())
                                 , widgetCursor = Nothing
                                 }
  in [pictureWidget, cleanupSubsWidget]


--------------------------------------------------------------------------------
-- | Allocate a new framebuffer and texture for our layer.
allocLayerFBTex
  :: V2 Int
  -- ^ Size of the buffer and texture needed.
  -> IO (GLuint, GLuint)
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
    putStrLn "allocLayerFBTex: could not complete the framebuffer!"
  glBindFramebuffer GL_FRAMEBUFFER 0
  return (fb, tex)


layerPic
  :: V2 Float
  -> GLuint
  -> TexturePicture ()
layerPic (V2 wf hf) tex = do
  setTextures [tex]
  setGeometry $ fan $ do
    to (0       , V2 0  0)
    to (V2 wf  0, V2 1  0)
    to (V2 wf hf, V2 1  (-1))
    to (V2 0  hf, V2 0  (-1))


-- | Renders the list of widgets into the layer's framebuffer and texture.
renderWidgetsIntoLayer
  :: Window
  -- ^ The SDL window this layer resides in.
  -> LayerInternal
  -- ^ The layer.
  -> IO ()
renderWidgetsIntoLayer window l = do
  let V4 r g b a = liBackgroundColor l
  -- Target the layer's framebuffer so all rendering goes there
  glBindFramebuffer GL_FRAMEBUFFER $ liFramebuffer l
  glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT
  -- Flip and translate the lower left y coordinate so we draw into our attached
  -- texture with the correct orientation
  fbsz@(V2 _ fhf :: V2 Double) <- fromIntegral <$$> glGetDrawableSize window
  let V2 _ wh  = fromIntegral <$> liSize l
      V2 fw fh = floor <$> fbsz
  glViewport 0 (floor $ -fhf + 2 * wh) fw fh
  -- Render all the widgets into the layer's framebuffer and by attachment,
  -- also its texture. Gather all the cursor requests.
  forM_ (liWidgets l) $ \w -> renderWidget w $ widgetTransform w
  -- Fix the viewport for later renderings
  glViewport 0 0 fw fh
  -- Target the default framebuffer
  glBindFramebuffer GL_FRAMEBUFFER 0
  return ()


data LayerUpdate = LayerSetAABB (V2 Float, V2 Float)
                 | LayerWidgets [Widget]


foldLayer
  :: MonadIO m
  => Window
  -> TVar Word64
  -> V2V2Renderer
  -> LayerInternal
  -> LayerUpdate
  -> m LayerInternal
foldLayer window tvFresh (V2V2Renderer v2v2) st = \case
  LayerSetAABB (tl, br) -> liftIO $ do
    let sz  = br - tl
        szi = floor <$> sz
    (fb, tex) <- allocLayerFBTex szi
    k         <- freshWith tvFresh
    picRend   <- snd <$> compilePicture v2v2 (layerPic sz tex)
    let st1 = st { liK               = k
                 , liSize            = szi
                 , liFramebuffer     = fb
                 , liTexture         = tex
                 , liPictureRenderer = picRend
                 }
    renderWidgetsIntoLayer window st1
    return st1
  LayerWidgets ws       -> do
    let st1 = st { liWidgets = ws }
    liftIO $ renderWidgetsIntoLayer window st1
    -- Manually release the old widgets
    let oldWs  = liWidgets st
        oldMap = M.fromList $ map (widgetUid &&& id) oldWs
    void $ freeStaleWidgets (oldMap, oldWs) ws
    return st1


--------------------------------------------------------------------------------
-- | A layer is a widget with its own translated and fenced off coordinate
-- system. It houses sub-widgets that are cut off from others. Only when
-- the user mouses within the layer will the sub-widgets be given any
-- user-aware events. Said in another way - sub-widget events are gated
-- by the layer's boundary and their spatial event data is transformed by the
-- layer's transformation.
layer
  :: OdinWidget r t m
  => Shape
  -- ^ The initial shape of the layer's boundary.
  -> V4 Float
  -- ^ The initial background color.
  -> LayerCfg t
  -- ^ Any event driven updates.
  -> DynamicWriterT t [Widget] m a
  -- ^ The subwidgets to run in this layer.
  -> m a
layer boundIni colorIni cfg subwidgets = do
  dMousePos <- holdDyn (-1/0) =<< getMousePositionEvent
  dShape    <- holdDyn boundIni $ cfg ^. setBoundaryEvent

  let dHasMouse = forDyn2 dShape dMousePos shapeHasPoint

  evMouseMotion <- getMouseMotionEvent
  evMouseButton <- getMouseButtonEvent
  evMouseWheel  <- getMouseWheelEvent

  let evLocalMouseMotion = gate (current dHasMouse) evMouseMotion
      evLocalMouseButton = gate (current dHasMouse) evMouseButton
      evLocalMouseWheel  = gate (current dHasMouse) evMouseWheel

  (a, dWidgets) <- local (\se -> se { sysMouseMotionEvent = evLocalMouseMotion
                                    , sysMouseButtonEvent = evLocalMouseButton
                                    , sysMouseWheelEvent  = evLocalMouseWheel
                                    }) $ runDynamicWriterT subwidgets

  let sz = fmap floor $ uncurry (flip (-)) $ shapeAABB boundIni
  window            <- getWindow
  tvFresh           <- getFreshVar
  V2V2Renderer v2v2 <- getV2V2
  initial           <- liftIO $ do
    lk <- freshWith tvFresh
    wk <- freshWith tvFresh
    (fb, tex) <- allocLayerFBTex sz
    picRend   <- snd <$> compilePicture v2v2 (layerPic (fromIntegral <$> sz) tex)
    return LayerInternal { liK = lk
                         , liWidgetsK = wk
                         , liFramebuffer = fb
                         , liTexture = tex
                         , liSize = sz
                         , liBackgroundColor = colorIni
                         , liBoundary = boundIni
                         , liWidgets = []
                         , liPictureRenderer = picRend
                         }

  let evAABB    = LayerSetAABB . shapeAABB <$> cfg ^. setBoundaryEvent
      evWidgets = LayerWidgets             <$> updated dWidgets
      evUpdate  = leftmost [evAABB, evWidgets]

  dLayer <- accumM (foldLayer window tvFresh $ V2V2Renderer v2v2) initial evUpdate
  tellDyn $ toWidgets <$> dLayer
  return a
