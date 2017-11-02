{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.New.UI.Layout
  ( measure
  , transformDyn
  , transform
  , transformMouseEvents
  , transformSubwidgets
  , alignWith
  , alignInRow
  , alignInCol
  , mapMDyn
  , captureW
  ) where

import           Control.Monad        (foldM)
import           Control.Monad.Trans  (lift)
import           Control.Monad.Reader (local, MonadFix)
import qualified Data.Vector.Unboxed  as V
import           Gelatin.GL
import           Reflex.SDL2          hiding (fan)
import           Reflex.SDL2.Internal

import           Odin.Engine.New


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


-- | Map a monadic function over the output of a 'DynamicWriterT'.
mapMDyn :: (Monoid w, Monoid w', Reflex t, MonadHold t m, MonadFix m)
        => (Dynamic t w -> m (Dynamic t w'))
        -> DynamicWriterT t w m a
        -> DynamicWriterT t w' m (a, Dynamic t w')
mapMDyn f dw = do
  (r, d) <- lift $ do
    (r, d) <- runDynamicWriterT dw
    d' <- f d
    return (r, d')
  tellDyn d
  return (r, d)


captureW
  :: (Monoid w, Reflex t, MonadHold t m, MonadFix m)
  => DynamicWriterT t w m a
  -> DynamicWriterT t w m (a, Dynamic t w)
captureW dw = do
  (r, d) <- lift $ runDynamicWriterT dw
  tellDyn d
  return (r, d)



transformDyn
  :: OdinWidget r t m
  => Dynamic t [RenderTransform2]
  -> DynamicWriterT t [Widget] m a
  -> m a
transformDyn dTfrm widgets = transformMouseEvents dTfrm $ do
  (a, dWidgets) <- runDynamicWriterT widgets
  let dWidgetsTd = zipDynWith transformWidgets dWidgets dTfrm
  tellDyn dWidgetsTd
  return a


transformSubwidgets
  :: OdinWidget r t m
  => Dynamic t [RenderTransform2]
  -> DynamicWriterT t [Widget] m a
  -> DynamicWriterT t [Widget] m a
transformSubwidgets dTfrm widgets = transformMouseEvents dTfrm $ do
  (a, dWidgets) <- captureW widgets
  tellDyn $ zipDynWith transformWidgets dWidgets dTfrm
  return a


transform
  :: OdinWidget r t m
  => [RenderTransform2]
  -> DynamicWriterT t [Widget] m a
  -> m a
transform ts = transformDyn (pure ts)


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
