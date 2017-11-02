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
  , setData
  , setButtonPainter
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


data ButtonInternal a = ButtonInternal { biK         :: Word64
                                       , biData      :: a
                                       , biMousePos  :: V2 Float
                                       , biState     :: ButtonState
                                       , biPainter   :: Painter (ButtonData a) IO
                                       , biPainting  :: ButtonState -> Painting
                                       }


mkPaintings :: Painter (ButtonData a) IO -> a -> IO (ButtonState -> Painting)
mkPaintings pntr a = do
  let runInputPainter = runPainter pntr . ButtonData a
      states          = [minBound .. maxBound]
  paintings <- mapM runInputPainter states
  return $ \st -> fromMaybe mempty $ lookup st $ zip states paintings


biBoundary :: ButtonInternal a -> Shape
biBoundary bi = paintingShape $ biPainting bi $ biState bi


biContainsMouse :: ButtonInternal a -> Bool
biContainsMouse bi = shapeHasPoint (biBoundary bi) (biMousePos bi)

biRender :: ButtonInternal a -> [RenderTransform2] -> IO ()
biRender bi = renderPainting $ biPainting bi $ biState bi


biFree :: ButtonInternal a -> IO ()
biFree bi = mapM_ (freePainting . biPainting bi) [minBound .. maxBound]


toWidget :: ButtonInternal a -> Widget
toWidget bi = Widget { widgetUid       = biK bi
                     , widgetBoundary  = [biBoundary bi]
                     , widgetTransform = []
                     , widgetCursor    = SDL_SYSTEM_CURSOR_HAND <$
                         guard (biState bi == ButtonStateOver)
                     , widgetRenderer2 = (biFree bi, biRender bi)
                     }


data ButtonUpdate a = ButtonUpdateSetData a
                    | ButtonUpdateSetPainter (Painter (ButtonData a) IO)
                    | ButtonUpdateMotion MouseMotionEventData
                    | ButtonUpdateButton MouseButtonEventData
                    | ButtonUpdateCycle


foldButton
  :: MonadIO m
  => TVar Word64
  -> ButtonInternal a
  -> ButtonUpdate a
  -> m (ButtonInternal a)
foldButton tvFresh bi up
  | ButtonUpdateSetData a <- up = liftIO $ do
    k  <- freshWith tvFresh
    ps <- mkPaintings (biPainter bi) a
    return bi {biK = k, biPainting = ps}

  | ButtonUpdateSetPainter pntr <- up = liftIO $ do
    k  <- freshWith tvFresh
    ps <- mkPaintings pntr (biData bi)
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
  :: forall r t m a. OdinWidget r t m
  => Painter (ButtonData a) IO
  -> a
  -> ButtonCfg t a
  -> m (Dynamic t ButtonState)
buttonWith buttonPainter a cfg = do
  tvFresh <- getFreshVar

  evMotion <- getMouseMotionEvent
  evButton <- getMouseButtonEvent

  mdo
    let evUpdate = leftmost [ ButtonUpdateSetData    <$> cfg ^. setData
                            , ButtonUpdateSetPainter <$> cfg ^. setButtonPainter
                            , ButtonUpdateMotion     <$> evMotion
                            , ButtonUpdateButton     <$> evButton
                            , ButtonUpdateCycle      <$  evAfterClick
                            ]
    initial <- liftIO $ do
      k  <- freshWith tvFresh
      ps <- mkPaintings buttonPainter a
      return ButtonInternal { biK    = k
                            , biData = a
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
  -> ButtonCfg t String
  -> m (Dynamic t ButtonState)
button str cfg = do
  pntr <- getButtonPainter
  buttonWith pntr str cfg


iconButton
  :: forall r t m. OdinWidget r t m
  => Char
  -> ButtonCfg t Char
  -> m (Dynamic t ButtonState)
iconButton char cfg = do
  pntr <- getIconButtonPainter
  buttonWith pntr char cfg


buttonClickedEvent :: Reflex t => Dynamic t ButtonState -> Event t ()
buttonClickedEvent = fmapMaybe (guard . (== ButtonStateClicked)) . updated
