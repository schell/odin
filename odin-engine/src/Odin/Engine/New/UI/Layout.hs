{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.New.UI.Layout
  ( measure
  , transform
  , transformMouseEvents
  , alignWith
  , alignInRow
  , alignInCol
  ) where

import           Control.Monad        (foldM)
import           Control.Monad.Reader (local)
import qualified Data.Vector.Unboxed  as V
import           Gelatin.GL
import           Reflex.SDL2          hiding (fan)
import           Reflex.SDL2.Internal

import           Odin.Engine.New


widgetsAABB :: [Widget] -> (V2 Float, V2 Float)
widgetsAABB =
  foldIntoBox . V.fromList . map shapeAABB . concatMap globalWidgetBoundary


measure
  :: OdinWidget r t m
  => DynamicWriterT t [Widget] m a
  -> m (a, Dynamic t (V2 Float, V2 Float))
measure widget = do
  (a, dWidgets) <- runDynamicWriterT widget
  tellDyn dWidgets
  return (a, widgetsAABB <$> dWidgets)


transformMouseEvents
  :: OdinWidget r t m
  => Dynamic t [RenderTransform2]
  -> m a
  -> m a
transformMouseEvents dTransform subwidgets = do
  evMouseMotion <- getMouseMotionEvent
  evMouseButton <- getMouseButtonEvent

  let transformMot ts dat
        | P ep <- fromIntegral <$> mouseMotionEventPos dat
        , er   <- fromIntegral <$> mouseMotionEventRelMotion dat
        , mv   <- inv44 $ affine2sModelview $ extractSpatial ts =
          dat { mouseMotionEventPos = P $ floor <$> transformV2 mv ep
              , mouseMotionEventRelMotion = floor <$> transformV2 mv er
              }
      evLocalMouseMotion =
        attachPromptlyDynWith transformMot dTransform evMouseMotion
      transformBtn ts dat
        | P ep <- fromIntegral <$> mouseButtonEventPos dat
        , mv   <- inv44 $ affine2sModelview $ extractSpatial ts =
          dat { mouseButtonEventPos = P $ floor <$> transformV2 mv ep }
      evLocalMouseButton =
        attachPromptlyDynWith transformBtn dTransform evMouseButton

  local (\se -> se { sysMouseMotionEvent = evLocalMouseMotion
                   , sysMouseButtonEvent = evLocalMouseButton
                   }) subwidgets


transform
  :: OdinWidget r t m
  => Dynamic t [RenderTransform2]
  -> DynamicWriterT t [Widget] m a
  -> m a
transform dTfrm widgets = transformMouseEvents dTfrm $ do
  (a, dWidgets) <- runDynamicWriterT widgets
  tellDyn $ zipDynWith transformWidgets dWidgets dTfrm
  return a


alignWith
  :: OdinWidget r t m
  => ((V2 Float, V2 Float) -> V2 Float)
  -> [DynamicWriterT t [Widget] m a]
  -> m [a]
alignWith toOffset ws = do
  (as, _) <- (\f -> foldM f ([], constDyn $ V2 0 0) ws) $ \(as, dxy) w -> do
    let dTfrm = pure . moveV2 <$> dxy
    transformMouseEvents dTfrm $ do
      (a, dWidgets) <- runDynamicWriterT w
      tellDyn $ zipDynWith transformWidgets dWidgets dTfrm
      let dxyNext = zipDynWith (+) dxy $ toOffset . widgetsAABB <$> dWidgets
      return (as ++ [a], dxyNext)
  return as


alignInRow
  :: OdinWidget r t m
  => [DynamicWriterT t [Widget] m a]
  -> m [a]
alignInRow = alignWith ((V2 1 0 *) . snd)


alignInCol
  :: OdinWidget r t m
  => [DynamicWriterT t [Widget] m a]
  -> m [a]
alignInCol = alignWith ((V2 0 1 *) . snd)
