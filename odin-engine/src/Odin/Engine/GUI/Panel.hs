{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.GUI.Panel
  ( PanelState(..)
  , Panel(..)
  , slotPanel
  , renderPanel
  ) where

import           Control.Monad                 (when)
import           Gelatin.SDL2
import           SDL                           hiding (get)
import           SDL.Raw.Enum
--------------------------------------------------------------------------------
import           Data.Char.FontAwesome
import           Odin.Engine
import           Odin.Engine.GUI.Button
import           Odin.Engine.GUI.Pane
import           Odin.Engine.GUI.Picture
import           Odin.Engine.GUI.Styles
import           Odin.Engine.GUI.Text.Internal
import           Odin.Engine.Slots
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
                   , pTrim   :: Slot Renderer2
                   , pTitle  :: Slot Text
                   , pClose  :: Slot Button
                   , pSize   :: V2 Int
                   , pOffset :: V2 Int
                   , pState  :: PanelState
                   , pId     :: Int
                   }

panelTrimPic :: V2 Int -> ColorPicture ()
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
  :: ( ReadsRenderers r
     , AltersUI r
     , AltersFontMap r
     , Member IO r
     , Member Allocates r
     , Member Fresh r
     , Member (Reader DefaultFont) r
     , Member (Reader IconFont) r
     )
  => String
  -> V2 Int
  -> V2 Int
  -> Eff r (Slot Panel)
slotPanel str size contentSize = do
  pane             <- slotPane (size - V2 0 20) contentSize 0
  DefaultFont font <- readDefaultFontDescriptor
  title            <- slotText font black str
  close            <- slotButton iconButtonPainter [faTimes]
  (_,bg)           <- slotColorPicture $ panelTrimPic size
  k                <- fresh
  slotVar $ Panel pane bg title close size 0 PanelStatePassive k

renderPanel
  :: (ReadsRenderers r, AltersUI r, Member IO r, Member Allocates r)
  => Slot Panel
  -> [RenderTransform2]
  -> (V2 Int -> Eff r a)
  -- | TODO: ^ Change this to V2 Float
  -> Eff r (a, PanelState)
renderPanel s rs f = do
  -- Render the panel just as it is, we have to do this in order to update the
  -- ui from the window pane
  a <- do
    p@Panel{..} <- unslot s
    let ts = moveV2 (fromIntegral <$> pOffset):rs
    renderPicture pTrim ts
    renderText pTitle $ move 21 16:ts
    shouldClose <-
      (==ButtonStateClicked) <$> renderButton pClose (move 3 (-1):ts)
    let paneOffset = V2 0 20
        finalState = if shouldClose then PanelStateShouldClose else pState
        uiblocked  = pState `elem` [PanelStateResizing, PanelStateDragging]
        localState = when uiblocked setUIBlocked
    (childUI, a) <- uiLocal (run . (snd <$>) . runState localState) $
      renderPane pPane (moveV2 paneOffset:ts) f
    when (uiActiveId childUI /= UiItemBlocked) $
      modify $ \ui -> ui{ uiActiveId = uiActiveId childUI
                        , uiSystemCursor = uiSystemCursor childUI
                        }
    reslot s p{pState=finalState}
    return a

  -- if the system can allow ui to happen, update the panel state
  canBeActive <- getCanBeActive
  finalState  <- do
    p@Panel{..} <- unslot s
    if canBeActive
      then do
        isDown <- queryMouseButton ButtonLeft
        rel <- uiMousePosRel <$> get
        case pState of
          PanelStateDragging -> do
            setSystemCursor SDL_SYSTEM_CURSOR_SIZEALL
            setUIActive pId
            if isDown
              then do
                reslot s p{pOffset=pOffset + rel}
                return PanelStateDragging

              else return PanelStateDropped
          PanelStateResizing -> do
            setSystemCursor SDL_SYSTEM_CURSOR_SIZEALL
            setUIActive pId
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
                barbb  = both
                              (transformV2 pmv)
                              (dxy + V2 16 0, dxy + V2 16 0 + V2 w 20)
                mposf = fromIntegral <$> mpos
                draggable = pointInBox mposf barbb
                rszbb = both (transformV2 pmv) (dxy + sz - 8, dxy + sz + 8)
                resizeable = pointInBox mposf rszbb
            if isDown && resizeable
              then do
                setUIActive pId
                setSystemCursor SDL_SYSTEM_CURSOR_SIZEALL
                return PanelStateResizing
              else if isDown && draggable
                     then do
                       setUIActive pId
                       setSystemCursor SDL_SYSTEM_CURSOR_SIZEALL
                       return PanelStateDragging
                     else do
                       when resizeable $
                         setSystemCursor SDL_SYSTEM_CURSOR_HAND
                       return PanelStatePassive
      else return pState
  modifySlot s $ \p -> p{pState=finalState}
  return (a, finalState)
