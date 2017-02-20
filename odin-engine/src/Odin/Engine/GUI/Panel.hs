{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE RecordWildCards  #-}
module Odin.Engine.GUI.Panel
  ( PanelState(..)
  , Panel(..)
  , slotPanel
  , renderPanel
  ) where

import           Gelatin.SDL2 hiding (both)
import           SDL
import           Odin.Engine.Eff
import           Odin.Engine.GUI.Common
import           Odin.Engine.GUI.Picture
import           Odin.Engine.GUI.Pane
import           Odin.Engine.GUI.Button
import           Odin.Engine.GUI.Text.Internal
import           Odin.Engine.GUI.Styles
import           Control.Lens (over, both, (.=), (^.))
import           Control.Monad.Trans.State.Strict
import           Data.Char.FontAwesome
--------------------------------------------------------------------------------
-- Panel
--------------------------------------------------------------------------------
data PanelState = PanelStatePassive
                | PanelStateDragging
                | PanelStateDropped
                | PanelStateResizing
                | PanelStateResized
                | PanelStateShouldClose
                deriving (Show, Eq)

data Panel os = Panel { pPane   :: Slot os (Pane os)
                      , pTrim   :: Slot os Renderer2
                      , pTitle  :: Slot os Text
                      , pClose  :: Slot os Button
                      , pSize   :: V2 Int
                      , pOffset :: V2 Int
                      , pState  :: PanelState
                      , pId     :: Int
                      }

panelTrimPic :: Monad m => V2 Int -> ColorPictureT m ()
panelTrimPic size = do
  let sz@(V2 w _) = fromIntegral <$> size
  setStroke [StrokeWidth 3, StrokeFeather 1]
  setGeometry $ do
    fan $ mapVertices (\v -> (v, fromHex 0x0000007f)) $
      rectangle 2 $ 2 + sz
    fan $ mapVertices (\v -> (v, V4 0.6 0.6 0.6 1)) $
      rectangle 0 $ V2 w 20
    line $ mapVertices (\v -> (v, V4 0.7 0.7 0.7 1)) $ do
      rectangle 0 $ sz + 1
      to 0

slotPanel
  :: GUI s m
  => String
  -> V2 Int
  -> V2 Int
  -> AllocatedT os m (Slot os (Panel os))
slotPanel str size contentSize = do
  pane   <- slotPane (size - V2 0 20) contentSize 0
  font   <- getDefaultFontDescriptor
  title  <- slotText font black str
  close  <- slotButton (iconButtonPainter 16) [faTimes]
  (_,bg) <- slotColorPicture $ panelTrimPic size
  k      <- fresh
  slot (Panel pane bg title close size 0 PanelStatePassive k) $ const $ return ()

renderPanel
  :: GUI s m
  => Slot os (Panel os)
  -> [RenderTransform2]
  -> (V2 Int -> m a)
  -> AllocatedT os m (a, PanelState)
renderPanel s rs f = do
  -- Render the panel just as it is, we have to do this in order to update the
  -- ui from the window pane
  a <- do
    p@Panel{..} <- unslot s
    let ts = move (fromIntegral <$> pOffset):rs
    renderPicture pTrim ts
    renderText pTitle $ move2 21 16:ts
    shouldClose <-
      (==ButtonStateClicked) <$> renderButton pClose (move2 3 (-1):ts)
    let paneOffset = V2 0 20
        finalState = if shouldClose then PanelStateShouldClose else pState
        uiblocked  = pState `elem` [PanelStateResizing, PanelStateDragging]
        localState = when uiblocked $ activeId .= UiItemBlocked
    (childUI, a) <- uiLocal (execState localState) $
      renderPane pPane (move paneOffset:ts) f
    when (childUI^.activeId /= UiItemBlocked) $ do
      ui.activeId .= childUI^.activeId
      ui.systemCursor .= childUI^.systemCursor
    reslot s p{pState=finalState}
    return a

  -- if the system can allow ui to happen, update the panel state
  canBeActive <- getCanBeActive
  finalState  <- do
    p@Panel{..} <- unslot s
    if canBeActive
      then do
        isDown <- queryMouseButton ButtonLeft
        rel <- use (ui . mousePosRel)
        case pState of
          PanelStateDragging -> do
            ui.systemCursor .= SDL_SYSTEM_CURSOR_SIZEALL
            setActive pId
            if isDown
              then do
                reslot s p{pOffset=pOffset + rel}
                return PanelStateDragging

              else return PanelStateDropped
          PanelStateResizing -> do
            ui.systemCursor .= SDL_SYSTEM_CURSOR_SIZEALL
            setActive pId
            if isDown
              then do
                let V2 sw sh = rel + pSize
                    sz = V2 (max 100 sw) (max 25 sh)
                reslotColorPicture pTrim $ panelTrimPic sz
                resizePane pPane (sz - V2 0 20)
                Pane{..} <- unslot pPane
                offsetPane pPane paneContentOffset
                reslot s p{pSize=sz}
                return PanelStateResizing
              else return PanelStateResized
          _ -> do
            mpos        <- getMousePosition
            let pmv = affine2sModelview $ extractSpatial rs
                sz@(V2 w _) = fromIntegral <$> pSize
                dxy = fromIntegral <$> pOffset
                barbb  = over both
                              (transformV2 pmv)
                              (dxy + V2 16 0, dxy + V2 16 0 + V2 w 20)
                mposf = fromIntegral <$> mpos
                draggable = pointInBox mposf barbb
                rszbb = over both (transformV2 pmv) (dxy + sz - 8, dxy + sz + 8)
                resizeable = pointInBox mposf rszbb
            if isDown && resizeable
              then do
                setActive pId
                ui.systemCursor .= SDL_SYSTEM_CURSOR_SIZEALL
                return PanelStateResizing
              else if isDown && draggable
                     then do
                       setActive pId
                       ui.systemCursor .= SDL_SYSTEM_CURSOR_SIZEALL
                       return PanelStateDragging
                     else do
                       when resizeable $
                         ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
                       return PanelStatePassive
      else return pState
  modifySlot s $ \p -> p{pState=finalState}
  return (a, finalState)
