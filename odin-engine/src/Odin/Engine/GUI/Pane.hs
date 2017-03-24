{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.GUI.Pane
  ( Pane(..)
  , slotPane
  , renderPane
  , resizePane
  , offsetPane
  ) where

import           Control.Monad           (when)
import           Gelatin.SDL2
import           Odin.Engine
import           Odin.Engine.GUI.Layer
import           Odin.Engine.GUI.Picture
import           Odin.Engine.Slots
import           SDL.Raw.Enum

fint :: V2 Int -> V2 Float
fint = (fromIntegral <$>)
--------------------------------------------------------------------------------
-- Pane
--------------------------------------------------------------------------------
data PaneState = PaneStatePassive
               | PaneStateScrolling
               | PaneStateScrolled
               deriving (Show, Eq)

data Pane = Pane { paneContentOffset    :: V2 Int
                 , paneContentSize      :: V2 Int
                 , paneHorizontalScroll :: Slot Renderer2
                 , paneVerticalScroll   :: Slot Renderer2
                 , paneLayer            :: Slot Layer
                 , paneState            :: PaneState
                 , paneId               :: Int
                 }

paneScrollbarColor :: V4 Float
paneScrollbarColor = V4 1 1 1 0.5

paneScrollbarExtent :: Float
paneScrollbarExtent = 16

paneVerticalScrollPic :: Float -> ColorPicture ()
paneVerticalScrollPic h = setGeometry $ fan $
  mapVertices (, paneScrollbarColor) $ rectangle 0 (V2 paneScrollbarExtent h)

paneHorizontalScrollPic :: Float -> ColorPicture ()
paneHorizontalScrollPic w = setGeometry $ fan $
  mapVertices (, paneScrollbarColor) $ rectangle 0 (V2 w 16)

-- | The minimum (but negative) offset the content should move in each dimension
-- of a window pane.
paneMaxContentOffset :: V2 Int -> V2 Int -> V2 Float
paneMaxContentOffset layerSize paneContentSize = V2 w h
  where w = max 0 w0
        h = max 0 h0
        V2 w0 h0 = fint paneContentSize - fint layerSize

-- | The suggested size of the horizontal and vertical scroll bars.
paneScrollSize :: V2 Int -> V2 Int -> V2 Float
paneScrollSize layerSize paneContentSize = V2 clampw clamph
  where clampw  = max 0 w
        clamph  = max 0 h
        V2 w h  = pane * (min 1 <$> (pane / content))
        pane    = fromIntegral <$> layerSize
        content = fromIntegral <$> paneContentSize

-- | The maximum distance the scrollbars should move in each dimension of a
-- window pane.
maxScrollBarPos :: V2 Int -> V2 Int -> V2 Float
maxScrollBarPos layerSize paneContentSize = fint layerSize - sbsize
  where sbsize = paneScrollSize layerSize paneContentSize

-- | The suggested position of the horizontal and vertical scroll bars.
scrollBarPos :: V2 Int -> V2 Int -> V2 Int -> V2 Float
scrollBarPos layerSize paneContentSize paneContentOffset = maxpos * percnt
  where maxpos = maxScrollBarPos layerSize paneContentSize
        minoff = paneMaxContentOffset layerSize paneContentSize
        offset = fint paneContentOffset
        percnt = fnan <$> (offset / minoff)
        fnan t = if isNaN t then 0 else t

mouseUnitsToContentOffset :: V2 Int -> V2 Int -> V2 Int -> V2 Int
mouseUnitsToContentOffset layerSize paneContentSize units =
  floor <$> (maxoff * percnt)
  where maxpos = maxScrollBarPos layerSize paneContentSize
        maxoff = paneMaxContentOffset layerSize paneContentSize
        percnt = fint units / maxpos

clampContentOffset :: V2 Int -> V2 Int -> V2 Int -> V2 Int
clampContentOffset layerSize paneContentSize (V2 x y) = newoffset
  where V2 mxx mxy = floor <$> paneMaxContentOffset layerSize paneContentSize
        newoffset = V2 (max 0 $ min mxx x) (max 0 $ min mxy y)

slotPane
  :: ( ReadsRenderers r
     , AltersUI r
     , Member Allocates r
     , Member IO r
     , Member Fresh r
     )
  => V2 Int
  -> V2 Int
  -> V4 Float
  -> Eff r (Slot Pane)
slotPane wsz csz color = do
  let V2 sw sh = paneScrollSize wsz csz
  (_, hscrl) <- slotColorPicture $ paneHorizontalScrollPic sw
  (_,vscrl) <- slotColorPicture $ paneVerticalScrollPic sh
  layer     <- slotLayer wsz color
  k         <- fresh
  slot (Pane 0 csz hscrl vscrl layer PaneStatePassive k) $ const $ return ()

resizePane
  :: (ReadsRenderers r, AltersUI r, Member Allocates r, Member IO r)
  => Slot Pane
  -> V2 Int
  -> Eff r ()
resizePane s size = do
  Pane{..} <- unslot s
  reslotLayer paneLayer size
  let V2 sw sh = paneScrollSize size paneContentSize
  reslotColorPicture paneHorizontalScroll $ paneHorizontalScrollPic sw
  reslotColorPicture paneVerticalScroll   $ paneVerticalScrollPic sh

offsetPane
  :: (ReadsRenderers r, AltersUI r, Member IO r)
  => Slot Pane
  -> V2 Int
  -> Eff r ()
offsetPane s offset0 = do
  p@Pane{..} <- unslot s
  Layer{..}  <- unslot paneLayer
  let offset = clampContentOffset layerSize paneContentSize offset0
  reslot s p{paneContentOffset=offset}

-- | Renders the pane giving the subrendering the content offset.
renderPane
  :: (ReadsRenderers r, AltersUI r, Member IO r)
  => Slot Pane
  -> [RenderTransform2]
  -> (V2 Int -> Eff r a)
  -> Eff r a
renderPane s rs f = do
  p@Pane{..} <- unslot s
  Layer{..}  <- unslot paneLayer

  -- determine the mouse position with respect to the layer origin
  mpos0       <- getMousePosition
  canActivate <- getCanBeActive
  let mv    = inv44 $ affine2sModelview $ extractSpatial rs
      mposf = transformV2 mv (fromIntegral <$> mpos0)
      -- determine if the mouse is over the layer
      bb          = (0, fromIntegral <$> layerSize)
      mouseIsOver = canActivate && pointInBox mposf bb
      -- determine if the mouse is over either scrollbar
      V2 dx dy = scrollBarPos layerSize paneContentSize paneContentOffset
      V2 hw vh = paneScrollSize layerSize paneContentSize
      ext  = paneScrollbarExtent
      vsbb = (V2 0 dy, V2 ext (dy + vh))
      hsbb = (V2 dx 0, V2 (dx + hw) ext)
      mouseIsOverScroll = canActivate &&
        (pointInBox mposf vsbb || pointInBox mposf hsbb)
      -- determine the local ui state to use for the layer
      uiblocked = paneState == PaneStateScrolling || mouseIsOverScroll
                                                  || not mouseIsOver
      localState = do
        modify $ \ui -> ui{ uiMousePos = floor <$> mposf }
        when uiblocked setUIBlocked
      -- a function to render the scrollbars
      renderScrollBars x y = do
        renderPicture paneHorizontalScroll $ move x 0:rs
        renderPicture paneVerticalScroll   $ move 0 y:rs
  -- Run the nested rendering function using the layer's ui state to
  -- account for the pane's affine transformation, as well as
  -- attempting to cancel UI activity if the mouse is outside of the visible
  -- pane area.
  (childUI, a) <- uiLocal (run . (snd <$>) . runState localState) $
    renderLayer paneLayer rs $ f $ (-1) * paneContentOffset
  -- update the outer ui with the possibly active id
  when (uiActiveId childUI /= UiItemBlocked) $
    modify $ \ui -> ui{ uiActiveId     = uiActiveId childUI
                      , uiSystemCursor = uiSystemCursor childUI
                      }

  case paneState of
    PaneStateScrolling -> do
      setUIActive paneId
      -- If the user still has the mouse down, scroll by the amount the mouse
      -- has moved, relative to its last position
      isDown <- queryMouseButton ButtonLeft
      if isDown
        then do rel <- uiMousePosRel <$> get
                let inc = mouseUnitsToContentOffset layerSize paneContentSize rel
                offsetPane s $ paneContentOffset + inc
                -- show the "clutching" hand
                setSystemCursor SDL_SYSTEM_CURSOR_SIZEALL
        else reslot s p{ paneState = PaneStateScrolled }
    _ ->
      -- When the mouse is over the scrollbars (and can activate),
      -- set the cursor to a hand if the user is over the scrollbars
      when mouseIsOverScroll $ do
        setSystemCursor SDL_SYSTEM_CURSOR_HAND
        isDown <- queryMouseButton ButtonLeft
        -- If the user is also holding down the left mouse, start scrolling
        -- next render
        when isDown $
          reslot s p{ paneState = PaneStateScrolling }

  -- Render the scrollbars
  renderScrollBars dx dy
  -- Return the result of the layer rendering
  return a
