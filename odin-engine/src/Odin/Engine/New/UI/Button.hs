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
module Odin.Engine.New.UI.Button where

import           Control.Applicative          (liftA2)
import           Control.Lens                 hiding (to)
import           Gelatin.GL
import           Reflex.SDL2                  hiding (fan)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Paint
import           Odin.Engine.New.UI.Painters


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


buttonRenderer2 :: ButtonInternal -> ButtonState -> Renderer2
buttonRenderer2 btn st = paintingRenderer $ paintingForButtonState btn st


--renderButton :: ButtonInternal -> [RenderTransform2] -> ButtonState -> IO ()
--renderButton btn ts = flip renderPainting ts . paintingForButtonState btn
--
--
--freeButton :: ButtonInternal -> IO ()
--freeButton (ButtonInternal up ovr down clicked) =
--  mapM_ freePainting [up, ovr, down, clicked]


----------------------------------------------------------------------
transformBounds
  :: [RenderTransform2] -> (V2 Float, V2 Float) -> (V2 Float, V2 Float)
transformBounds ts (tl, br) = (transformV2 mv tl, transformV2 mv br)
  where mv = affine2sModelview $ extractSpatial ts


globalPointIsInsideButton
  :: ButtonInternal -> ButtonState -> [RenderTransform2] -> V2 Float -> Bool
globalPointIsInsideButton btn st ts p = pointInBox p (transformBounds ts bounds)
  where pnt    = paintingForButtonState btn st
        bounds = paintingBounds pnt


data ButtonUpdate = ButtonUpdateMotion MouseMotionEventData
                  | ButtonUpdateButton MouseButtonEventData
                  | ButtonUpdateNone


data ButtonStep = ButtonStep { bsInternal :: ButtonInternal
                             , bsMousePos :: V2 Float
                             , bsTfrms    :: [RenderTransform2]
                             , bsUpdate   :: ButtonUpdate
                             }


data ButtonOutput t =
  ButtonOutput { buttonOutputBounds :: Dynamic t (V2 Float, V2 Float)
               , buttonOutputState  :: Dynamic t ButtonState
               }


stepButton :: ButtonStep -> ButtonState -> ButtonState
stepButton (ButtonStep btn pos ts update) bstate
  | ButtonUpdateNone <- update = bstate

  | ButtonUpdateMotion _ <- update
  , ButtonStateUp == bstate
  , globalPointIsInsideButton btn bstate ts pos = ButtonStateOver

  | ButtonUpdateMotion _ <- update
  , ButtonStateOver == bstate
  , not $ globalPointIsInsideButton btn bstate ts pos = ButtonStateUp

  | ButtonUpdateButton dat <- update
  , ButtonStateOver == bstate
  , ButtonLeft      == mouseButtonEventButton dat
  , 1               == mouseButtonEventClicks dat
  , Pressed         == mouseButtonEventMotion dat = ButtonStateDown

  | ButtonUpdateButton dat <- update
  , ButtonStateDown == bstate
  , ButtonLeft      == mouseButtonEventButton dat
  , Released        == mouseButtonEventMotion dat =
    if globalPointIsInsideButton btn bstate ts pos
    then ButtonStateClicked
    else ButtonStateUp

  | ButtonStateClicked == bstate
  , ButtonUpdateMotion _ <- update =
    if globalPointIsInsideButton btn bstate ts pos
    then ButtonStateOver
    else ButtonStateUp

  | ButtonStateClicked == bstate
  , ButtonUpdateButton dat <- update
  , ButtonLeft == mouseButtonEventButton dat
  , Pressed    == mouseButtonEventMotion dat = ButtonStateDown

  | otherwise = bstate


button :: forall r t m. OdinWidget r t m => ButtonCfg t -> m (ButtonOutput t)
button cfg0 = do
  buttonPainter <- getButtonPainter
  evPB          <- getPostBuild
  let cfg = cfg0 & setButtonPainterEvent %~
            leftmost . (:[buttonPainter <$ evPB])

  tvFresh <- getFreshVar

  dTfrm     <- holdDyn [] (cfg ^. setTransformEvent)
  dMayText  <- holdDyn Nothing $ Just <$> (cfg ^. setTextEvent)
  dMayPaint <- holdDyn Nothing $ Just <$> (cfg ^. setButtonPainterEvent)

  let dMayNeeds :: Dynamic t (Maybe ButtonNeeds)
      dMayNeeds = forDyn2 dMayText dMayPaint $ liftA2 ButtonNeeds
      evNeeds :: Event t ButtonNeeds
      evNeeds = fmapMaybe id $ updated dMayNeeds
      mkWidget (ButtonNeeds txt p) = do
        k   <- freshWith tvFresh
        btn <- ButtonInternal <$> paint p (ButtonData txt ButtonStateUp)
                              <*> paint p (ButtonData txt ButtonStateOver)
                              <*> paint p (ButtonData txt ButtonStateDown)
                              <*> paint p (ButtonData txt ButtonStateClicked)
        let layerf :: [RenderTransform2] -> ButtonState -> WidgetTree
            -- TODO: Make sure we write the boundaries instead of '[]'
            layerf ts st = WidgetTreeLeaf k ts [] $ buttonRenderer2 btn st
        return (btn, layerf)

  evBtnMkWidget <- performEvent $ mkWidget <$> evNeeds
  dMkWidget     <- holdDyn (\_ _ -> mempty) $ snd <$> evBtnMkWidget
  dMayBInt      <- holdDyn Nothing $ Just . fst <$> evBtnMkWidget

  evMotion      <- getMouseMotionEvent
  evButton      <- getMouseButtonEvent
  let evUpdate = leftmost [ ButtonUpdateMotion <$> evMotion
                          , ButtonUpdateButton <$> evButton
                          ]
  dUpdate       <- holdDyn ButtonUpdateNone evUpdate
  dMousePos     <- holdDyn ((-1)/0) =<< getMousePositionEvent
  let evStep = fmapMaybe id $ updated $
        forDyn4 dMayBInt dMousePos dTfrm dUpdate $ \case
          Nothing -> \_ _ _ -> Nothing
          Just b  -> \p t u -> Just $ ButtonStep b p t u

  dState      <- foldDyn stepButton ButtonStateUp evStep
  dUniqState  <- holdUniqDyn dState
  dMkPainting <- holdDyn mempty $ paintingForButtonState . fst <$> evBtnMkWidget
  let dBounds = paintingBounds <$> zipDynWith ($) dMkPainting dUniqState
  tellDyn $ forDyn3 dTfrm dState dMkWidget $ \ts st mk -> mk ts st
  return $ ButtonOutput (zipDynWith transformBounds dTfrm dBounds) dUniqState
