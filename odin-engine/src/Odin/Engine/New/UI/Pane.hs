{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.New.UI.Pane
  ( pane
  , PaneCfg (..)
  ) where

import           Control.Monad               (msum, guard)
import           Control.Monad.Trans         (lift)
import           Data.Word                   (Word64)
import           Gelatin.SDL2                hiding (rotate, scale)
import           Reflex.SDL2                 hiding (fan)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Layer
import           Odin.Engine.New.UI.Layout
import           Odin.Engine.New.UI.Button
import           Odin.Engine.New.UI.Painters (getBlankButtonPainter)


fint :: V2 Int -> V2 Float
fint = (fromIntegral <$>)


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


--------------------------------------------------------------------------------
data PaneState = PaneStatePassive
               | PaneStateScrolling
               | PaneStateScrolled
               deriving (Show, Eq)

data PaneInternal = PaneInternal { piK             :: Word64
                                 , piWidgets       :: [Widget]
                                 , piContentOffset :: V2 Int
                                 , piHorScroll     :: Renderer2
                                 , piVerScroll     :: Renderer2
                                 , piState         :: PaneState
                                 }

data PaneUpdate = PaneUpdateWidgets [Widget]


toWidgets
 :: PaneInternal
 -> [Widget]
toWidgets p = [Widget { widgetUid       = piK p
                      , widgetTransform = []
                      , widgetBoundary  = concatMap widgetBoundary (piWidgets p)
                      , widgetRenderer2 = mconcat $ map widgetRenderer2 $ piWidgets p
                      , widgetCursor    = msum $ reverse $ map widgetCursor $ piWidgets p
                      }
              ]


pane
  :: forall r t m a. OdinWidget r t m
  => Shape
  -- ^ The initial shape of the layer's boundary.
  -> V4 Float
  -- ^ The initial background color.
  -> V2 Float
  -- ^ The initial content offset.
  -> PaneCfg t
  -- ^ Any event based updates.
  -> DynamicWriterT t [Widget] m a
  -- ^ The widgets to run within the pane.
  -> m a
pane boundIni colorIni scrollIni paneCfg subwidgets = do
  let layerCfg = def & setBoundaryEvent .~ (paneCfg ^. setBoundaryEvent)

  dBound    <- holdDyn boundIni  $ paneCfg ^. setBoundaryEvent
  dOffsetV2 <- holdDyn scrollIni $ paneCfg ^. setOffsetEvent

  -- subwidgets layer
  (a, dWidgetsAABB) <- layer boundIni colorIni layerCfg $ do
      (a, dWidgets) <- captureW $
        transformSubwidgets (pure . moveV2 <$> dOffsetV2) subwidgets
      return (a, widgetsAABB <$> dWidgets)

  btnPntr <- getBlankButtonPainter

  -- vertical scroll bar
  dVst          <- transform [moveV2 2] (buttonWith btnPntr (V2 10 100) def)
  dVIsScrolling <- (holdUniqDyn =<<) .
    holdDyn False $ (== ButtonStateDown) <$> updated dVst

  -- horizontal scroll bar
  dHst          <- transform [move 4 4] (buttonWith btnPntr (V2 100 10) def)
  dHIsScrolling <- (holdUniqDyn =<<) .
    holdDyn False $ (== ButtonStateDown) <$> updated dHst

  putDebugLnE (updated dVIsScrolling) $ ("v:" ++) . show
  putDebugLnE (updated dHIsScrolling) $ ("v:" ++) . show
  putDebugLnE (updated dWidgetsAABB)  $ ("aabb:" ++) . show

  return a
