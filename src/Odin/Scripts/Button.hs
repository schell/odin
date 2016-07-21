{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.Scripts.Button (ButtonData(..), ButtonState(..), freshButton) where

import Gelatin.SDL2
import SDL.Input.Mouse (MouseButton(..)
                       ,getMouseButtons
                       )

import Odin.Core

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 | ButtonStateClicked
                 deriving (Show, Eq, Ord, Enum, Bounded)

data ButtonData = ButtonData { btnPainterFont      :: FontData
                             , btnPainterStr       :: String
                             , btnPainterPointSize :: Float
                             , btnPainterFunc      :: FontData
                                                   -> String
                                                   -> Float
                                                   -> ButtonState
                                                   -> Pic
                             }

paintButton :: ButtonData -> ButtonState -> Pic
paintButton ButtonData{..} =
  btnPainterFunc btnPainterFont btnPainterStr btnPainterPointSize

data ButtonRndrs = ButtonRndrs { btnRndrsUp   :: RenderIO
                               , btnRndrsOver :: RenderIO
                               , btnRndrsDown :: RenderIO
                               }

buttonDown :: Mailbox ButtonState -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonDown mb sz rs btn = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  if not isDown
    then do isStillOver <- getMouseIsOverEntityWithSize btn sz
            send mb $ if isStillOver
              then ButtonStateClicked
              else ButtonStateUp
            btn `setRenderer` btnRndrsUp rs
            buttonUp mb sz rs btn
    else nextScript $ buttonDown mb sz rs btn

buttonOver :: Mailbox ButtonState -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonOver mb sz rs btn = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then if isDown then do btn `setRenderer` btnRndrsDown rs
                                   send mb ButtonStateDown
                                   buttonDown mb sz rs btn
                           else nextScript $ buttonOver mb sz rs btn
            else do btn `setRenderer` btnRndrsUp rs
                    send mb ButtonStateUp
                    buttonUp mb sz rs btn

buttonUp :: Mailbox ButtonState -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonUp mb sz rs btn = do
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then do btn `setRenderer` btnRndrsOver rs
                    send mb ButtonStateOver
                    buttonOver mb sz rs btn
            else nextScript $ buttonUp mb sz rs btn

-- | Creates a fresh button.
freshButton :: (DoesIO m
               ,MakesEntities m
               ,Reads Rez m
               ,ModifiesComponent RenderIO m
               ,ModifiesComponent DeallocIO m
               ,ModifiesComponent PictureTransform m
               ,ModifiesComponent [Script] m
               ) => ButtonData -> V2 Float -> Mailbox ButtonState -> m Entity
freshButton btn@ButtonData{..} pos mb = do
  let sz = pictureSize $ paintButton btn ButtonStateUp
  up   <- allocPicRenderer $ paintButton btn ButtonStateUp
  over <- allocPicRenderer $ paintButton btn ButtonStateOver
  down <- allocPicRenderer $ paintButton btn ButtonStateDown
  let dalloc = mapM_ fst [up,over,down]
      rs      = ButtonRndrs (snd up) (snd over) (snd down)
  actor <- fresh
  actor `setDealloc` dalloc
  actor `setRenderer` btnRndrsUp rs
  actor `setPicTransform` PictureTransform (Transform pos 1 0) 1 1
  actor `addScript` buttonUp mb sz rs actor
  return actor
