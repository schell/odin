{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Odin.Engine.New.UI.Button
  ( buttonWith
  , button
  , iconButton
  , buttonClickedEvent
  , ButtonState (..)
  , ButtonData (..)
  , ButtonCfg (..)
  , (^.)
  , (.~)
  , (&)
  , setTextEvent
  , setButtonPainterEvent
  , def
  ) where

import           Control.Applicative         ((<$))
import           Control.Lens                hiding (to)
import           Control.Monad               (guard)
import           Data.Maybe                  (fromMaybe)
import           Data.Word                   (Word64)
import           Gelatin.GL
import           Reflex.SDL2                 hiding (fan)
import           SDL.Raw.Enum                (pattern SDL_SYSTEM_CURSOR_HAND)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Painters
import           Odin.Engine.New.UI.Painting


data ButtonInternal = ButtonInternal { biK         :: Word64
                                     , biText      :: String
                                     , biMousePos  :: V2 Float
                                     , biState     :: ButtonState
                                     , biPainter   :: Painter ButtonData IO
                                     , biPainting  :: ButtonState -> Painting
                                     }


mkPaintings :: Painter ButtonData IO -> String -> IO (ButtonState -> Painting)
mkPaintings pntr str = do
  let runInputPainter = runPainter pntr . ButtonData str
      states          = [minBound .. maxBound]
  paintings <- mapM runInputPainter states
  return $ \st -> fromMaybe mempty $ lookup st $ zip states paintings


biBoundary :: ButtonInternal -> Shape
biBoundary bi = paintingShape $ biPainting bi $ biState bi


biContainsMouse :: ButtonInternal -> Bool
biContainsMouse bi = shapeHasPoint (biBoundary bi) (biMousePos bi)

biRender :: ButtonInternal -> [RenderTransform2] -> IO ()
biRender bi = renderPainting $ biPainting bi $ biState bi


biFree :: ButtonInternal -> IO ()
biFree bi = mapM_ (freePainting . biPainting bi) [minBound .. maxBound]


toWidget :: ButtonInternal -> Widget
toWidget bi = Widget { widgetUid       = biK bi
                     , widgetBoundary  = [biBoundary bi]
                     , widgetTransform = []
                     , widgetCursor    = SDL_SYSTEM_CURSOR_HAND <$
                         guard (biState bi == ButtonStateOver)
                     , widgetRenderer2 = (biFree bi, biRender bi)
                     }


data ButtonUpdate = ButtonUpdateSetText String
                  | ButtonUpdateSetPainter (Painter ButtonData IO)
                  | ButtonUpdateMotion MouseMotionEventData
                  | ButtonUpdateButton MouseButtonEventData
                  | ButtonUpdateCycle


foldButton
  :: MonadIO m
  => TVar Word64
  -> ButtonInternal
  -> ButtonUpdate
  -> m ButtonInternal
foldButton tvFresh bi up
  | ButtonUpdateSetText str <- up = liftIO $ do
    k  <- freshWith tvFresh
    ps <- mkPaintings (biPainter bi) str
    return bi {biK = k, biPainting = ps}

  | ButtonUpdateSetPainter pntr <- up = liftIO $ do
    k  <- freshWith tvFresh
    ps <- mkPaintings pntr (biText bi)
    return bi {biK = k, biPainting = ps, biPainter = pntr}

  | ButtonUpdateMotion dat <- up
  , P v2Cint               <- mouseMotionEventPos dat
  , bi1                    <- bi {biMousePos = fromIntegral <$> v2Cint} =
    if biState bi `elem` [ButtonStateUp, ButtonStateOver, ButtonStateClicked]
    then return bi1 { biState = if biContainsMouse bi1
                                then ButtonStateOver
                                else ButtonStateUp
                    }
    else return bi1

  | ButtonUpdateButton dat <- up
  , ButtonStateOver        <- biState bi
  , ButtonLeft             <- mouseButtonEventButton dat
  , Pressed                <- mouseButtonEventMotion dat =
    return bi { biState = ButtonStateDown }

  | ButtonUpdateButton dat <- up
  , ButtonStateDown        <- biState bi
  , ButtonLeft             <- mouseButtonEventButton dat
  , Released               <- mouseButtonEventMotion dat
  , P v2Cint               <- mouseButtonEventPos    dat
  , bi1                    <- bi {biMousePos = fromIntegral <$> v2Cint} =
    return bi1 { biState = if biContainsMouse bi1
                           then ButtonStateClicked
                           else ButtonStateUp
               }

  | otherwise = return bi { biState = if biContainsMouse bi
                                      then ButtonStateOver
                                      else ButtonStateUp
                          }


buttonWith
  :: forall r t m. OdinWidget r t m
  => Painter ButtonData IO
  -> String
  -> ButtonCfg t
  -> m (Dynamic t ButtonState)
buttonWith buttonPainter str cfg = do
  tvFresh <- getFreshVar

  evMotion <- getMouseMotionEvent
  evButton <- getMouseButtonEvent

  mdo
    let evUpdate = leftmost [ ButtonUpdateSetText    <$> cfg ^. setTextEvent
                            , ButtonUpdateSetPainter <$> cfg ^. setButtonPainterEvent
                            , ButtonUpdateMotion     <$> evMotion
                            , ButtonUpdateButton     <$> evButton
                            , ButtonUpdateCycle      <$  evAfterClick
                            ]
    initial <- liftIO $ do
      k  <- freshWith tvFresh
      ps <- mkPaintings buttonPainter str
      return ButtonInternal { biK = k
                            , biText = str
                            , biMousePos = -1/0
                            , biState = ButtonStateUp
                            , biPainter = buttonPainter
                            , biPainting = ps
                            }

    dInternal <- accumM (foldButton tvFresh) initial evUpdate
    tellDyn $ pure . toWidget <$> dInternal
    dState <- holdUniqDyn $ biState <$> dInternal
    let evClicked = fmapMaybe (guard . (== ButtonStateClicked)) $ updated dState
    kcode        <- fresh
    evAfterClick <- delayEventWithEventCode (fromIntegral kcode) 1 evClicked
    return dState


button
  :: forall r t m. OdinWidget r t m
  => String
  -> ButtonCfg t
  -> m (Dynamic t ButtonState)
button str cfg = do
  pntr <- getButtonPainter
  buttonWith pntr str cfg


iconButton
  :: forall r t m. OdinWidget r t m
  => Char
  -> ButtonCfg t
  -> m (Dynamic t ButtonState)
iconButton char cfg = do
  pntr <- getIconButtonPainter
  buttonWith pntr [char] cfg

buttonClickedEvent :: Reflex t => Dynamic t ButtonState -> Event t ()
buttonClickedEvent = fmapMaybe (guard . (== ButtonStateClicked)) . updated
