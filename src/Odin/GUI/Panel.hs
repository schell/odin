{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE RecordWildCards  #-}
module Odin.GUI.Panel
  ( PanelState(..)
  , Panel(..)
  , allocPanel
  , renderPanel
  ) where

import           Gelatin.SDL2 hiding (move, scale, rotate)
import           SDL
import           Odin.Core
import           Odin.GUI.Picture
import           Odin.GUI.Pane
import           Odin.GUI.Button
import           Odin.GUI.Text.Internal
import           Odin.GUI.Styles
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

data Panel = Panel { pPane   :: Slot Pane
                   , pTrim   :: Slot GUIRenderer
                   , pTitle  :: Slot Text
                   , pClose  :: Slot Button
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
      rectangle 0 sz
      to 0

allocPanel :: GUI s m => String -> V2 Int -> V2 Int -> m (Slot Panel)
allocPanel str size contentSize = do
  pane   <- allocPane (size - V2 0 20) contentSize 0
  font   <- getDefaultFontDescriptor
  title  <- allocText font black str
  close  <- allocButton (iconButtonPainter 16) [faTimes]
  (_,bg) <- allocColorPicture $ panelTrimPic size
  k      <- fresh
-- registerFree -- Nothing to register
  slot $ Panel pane bg title close size 0 PanelStatePassive k

renderPanel :: GUI s m
            => Slot Panel -> [RenderTransform] -> (V2 Int -> m a)
            -> m (a, PanelState)
renderPanel s rs f = do
  -- render the panel just as it is, we have to do this in order to update the
  -- ui from the window pane
  a <- do
    p@Panel{..} <- unslot s
    let ts = moveV2 (fromIntegral <$> pOffset):rs
    renderPicture pTrim ts
    renderText pTitle $ move 21 16:ts
    shouldClose <- (==ButtonStateClicked) <$>
      (renderButton pClose $ move 3 (-1):ts)
    let paneOffset = V2 0 20
        finalState = if shouldClose then PanelStateShouldClose else pState
        uiblocked  = any (pState ==) [PanelStateResizing, PanelStateDragging]
        localState = when uiblocked $ activeId .= UiItemBlocked
    (childUI, a) <- uiLocal (execState localState) $
      renderPane pPane (moveV2 paneOffset:ts) f
    when (childUI^.activeId /= UiItemBlocked) $ do
      ui.activeId .= childUI^.activeId
      ui.systemCursor .= childUI^.systemCursor
    swapSlot s p{pState=finalState}
    return a

  -- if the system can allow ui to happen, update the panel state
  finalState  <- do
    canBeActive <- getCanBeActive
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
                swapSlot s p{pOffset=pOffset + rel}
                return PanelStateDragging

              else return PanelStateDropped
          PanelStateResizing -> do
            ui.systemCursor .= SDL_SYSTEM_CURSOR_SIZEALL
            setActive pId
            if isDown
              then do
                let V2 sw sh = rel + pSize
                    sz = V2 (max 100 sw) (max 25 sh)
                reallocColorPicture pTrim $ panelTrimPic sz
                resizePane pPane (sz - V2 0 20)
                Pane{..} <- unslot pPane
                offsetPane pPane paneContentOffset
                swapSlot s p{pSize=sz}
                return PanelStateResizing
              else return PanelStateResized
          _ -> do
            mpos        <- getMousePosition
            let pmv = affine2sModelview $ extractSpatial rs
                sz@(V2 w h) = fromIntegral <$> pSize
                dxy = fromIntegral <$> pOffset
                barbb  = over both
                              (transformV2 pmv)
                              (dxy + V2 16 0, dxy + V2 16 0 + V2 w 20)
                mposf = fromIntegral <$> mpos
                draggable = pointInBounds mposf barbb
                rszbb = over both (transformV2 pmv) (dxy + sz - 8, dxy + sz + 8)
                resizeable = pointInBounds mposf rszbb
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
