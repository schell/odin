{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import           Control.Applicative          (liftA2)
import           Control.Lens                 hiding (to)
import           Control.Monad                (void)
import           Data.Default                 (def)
import           Gelatin.GL
import           Reflex.SDL2                  hiding (fan)
import           System.Exit                  (exitSuccess)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Paint
import           Odin.Engine.New.UI.Picture
import           Odin.Engine.New.UI.TextField


data ButtonNeeds = ButtonNeeds String (Painter ButtonData IO)


data ButtonInternal = ButtonInternal { biUp      :: Painting
                                     , biOver    :: Painting
                                     , biDown    :: Painting
                                     , biClicked :: Painting
                                     }


paintingForButtonState :: ButtonInternal -> ButtonState -> Painting
paintingForButtonState btn = \case
  ButtonStateUp      -> biUp      btn
  ButtonStateOver    -> biOver    btn
  ButtonStateDown    -> biDown    btn
  ButtonStateClicked -> biClicked btn


renderButton :: ButtonInternal -> [RenderTransform2] -> ButtonState -> IO ()
renderButton btn ts = flip renderPainting ts . paintingForButtonState btn


freeButton :: ButtonInternal -> IO ()
freeButton (ButtonInternal up ovr down clicked) =
  mapM_ freePainting [up, ovr, down, clicked]


globalPointIsInsideButton
  :: ButtonInternal -> ButtonState -> [RenderTransform2] -> V2 Float -> Bool
globalPointIsInsideButton btn st ts p =
  let mv    = affine2sModelview $ extractSpatial ts
      pnt   = paintingForButtonState btn st
      (tl,br) = paintingBounds pnt
      gbnds   = (transformV2 mv tl, transformV2 mv br)
  in pointInBox p gbnds


data ButtonStep = ButtonStep { bsInternal :: ButtonInternal
                             , bsMotion   :: Maybe MouseMotionEventData
                             , bsButton   :: Maybe MouseButtonEventData
                             }


stepButton :: ButtonStep -> ButtonState -> ButtonState
stepButton = undefined


button :: forall r t m. OdinLayered r t m => ButtonCfg t -> m ()
button cfg = do
  tvFresh <- getFreshVar

  dTfrm     <- holdDyn [] (cfg ^. setTransformEvent)
  dMayText  <- holdDyn Nothing $ Just <$> (cfg ^. setTextEvent)
  dMayPaint <- holdDyn Nothing $ Just <$> (cfg ^. setButtonPainterEvent)

  let dMayNeeds :: Dynamic t (Maybe ButtonNeeds)
      dMayNeeds = forDyn2 dMayText dMayPaint $ liftA2 ButtonNeeds
      evNeeds :: Event t ButtonNeeds
      evNeeds = fmapMaybe id $ updated dMayNeeds
      mkLayer (ButtonNeeds txt p) = do
        k   <- freshWith tvFresh
        btn <- ButtonInternal <$> paint p (ButtonData txt ButtonStateUp)
                              <*> paint p (ButtonData txt ButtonStateOver)
                              <*> paint p (ButtonData txt ButtonStateDown)
                              <*> paint p (ButtonData txt ButtonStateClicked)
        let layerf :: [RenderTransform2] -> ButtonState -> [Layer]
            layerf ts st = [Layer k (renderButton btn ts st) (freeButton btn)]
        return (btn, layerf)

  evBtnMkLayer <- performEvent $ mkLayer <$> evNeeds
  dMkLayer     <- holdDyn (\_ _ -> []) $ snd <$> evBtnMkLayer
  dMayBInt     <- holdDyn Nothing $ Just . fst <$> evBtnMkLayer
  dMayMotion   <- holdDyn Nothing . (Just <$>) =<< getMouseMotionEvent
  dMayButton   <- holdDyn Nothing . (Just <$>) =<< getMouseButtonEvent

  let dStateInputs  = forDyn3 dMayBInt dMayMotion dMayButton (,,)
      evStateInputs = flip fmapMaybe (updated dStateInputs) $ \case
        (Nothing, _, _)       -> Nothing
        (Just nt, mmot, mbtn) -> Just $ ButtonStep nt mmot mbtn
  dState <- foldDyn stepButton ButtonStateUp evStateInputs
  commitLayers $ forDyn3 dTfrm dState dMkLayer $ \ts st mk -> mk ts st


guest :: forall r t m. (OdinLayered r t m) => m ()
guest = do
  DefaultFont fdesc <- getDefaultFont
  evPB              <- getPostBuild
  evMotion          <- getMouseMotionEvent
  evButton          <- getMouseButtonEvent
  dPos              <- holdDyn 0 $ ffor evMotion $ \motion ->
                         let P v = mouseMotionEventPos motion
                         in fromIntegral <$> v
  let evTFTfrms = leftmost [ pure . moveV2 <$> updated dPos
                           , [move 100 100] <$ evPB
                           ]
  void $ flip holdView (return () <$ evButton) $
    void $ textField $ def & setTextEvent           .~ ("Hello"    <$ evPB)
                           & setColorEvent          .~ (V4 1 1 1 1 <$ evPB)
                           & setFontDescriptorEvent .~ (fdesc      <$ evPB)
                           & setTransformEvent      .~ evTFTfrms

  let dPicPos = (V2 100 100 +) <$> dPos
      pic = setGeometry $ fan $ do
        to (            -100, V4 1 1 0 1)
        to (V2    100 (-100), V4 0 1 1 1)
        to (             100, V4 1 0 1 1)
        to (V2 (-100)    100, V4 1 1 1 1)
  colorPicture $ def & setTransformEvent .~ updated (pure . moveV2 <$> dPicPos)
                     & setPictureEvent   .~ (pic <$ evPB)

  getQuitEvent >>= performEvent_ . (liftIO exitSuccess <$)


main :: IO ()
main = runOdin () $ mainLoop guest
