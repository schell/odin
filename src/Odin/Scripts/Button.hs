{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.Scripts.Button (ButtonData(..), ButtonState(..), freshButton) where

import Gelatin.SDL2
import Control.Lens
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
            rndrs.at btn .= Just (btnRndrsUp rs)
            buttonUp mb sz rs btn
    else nextScript $ buttonDown mb sz rs btn

buttonOver :: Mailbox ButtonState -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonOver mb sz rs btn = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then if isDown then do rndrs.at btn .= Just (btnRndrsDown rs)
                                   send mb ButtonStateDown
                                   buttonDown mb sz rs btn
                           else nextScript $ buttonOver mb sz rs btn
            else do rndrs.at btn .= Just (btnRndrsUp rs)
                    send mb ButtonStateUp
                    buttonUp mb sz rs btn

buttonUp :: Mailbox ButtonState -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonUp mb sz rs btn = do
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then do rndrs.at btn .= Just (btnRndrsOver rs)
                    send mb ButtonStateOver
                    buttonOver mb sz rs btn
            else nextScript $ buttonUp mb sz rs btn

-- | Creates a fresh button.
freshButton :: ButtonData -> V2 Float -> Mailbox ButtonState -> System Entity
freshButton btn@ButtonData{..} pos mb = do
  let sz = pictureSize $ paintButton btn ButtonStateUp
  up   <- allocPicRenderer $ paintButton btn ButtonStateUp
  ovr  <- allocPicRenderer $ paintButton btn ButtonStateOver
  down <- allocPicRenderer $ paintButton btn ButtonStateDown
  let d  = mapM_ fst [up,ovr,down]
      rs = ButtonRndrs (snd up) (snd ovr) (snd down)
  k <- fresh
  k .# dloc d
    ## rndr (btnRndrsUp rs)
    ## tfrm (PictureTransform (Transform pos 1 0) 1 1)
    ## script [Script $ buttonUp mb sz rs k]
