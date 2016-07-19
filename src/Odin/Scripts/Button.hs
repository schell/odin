{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.Scripts.Button (ButtonData(..), ButtonState(..), freshButton) where

import Linear.Affine (Point(..))
import Gelatin.SDL2
import Control.Monad (when)
import SDL.Input.Mouse (MouseButton(..)
                       ,getAbsoluteMouseLocation
                       ,getMouseButtons
                       )

import Odin.Common
import Odin.Component
import Odin.System

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
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

getMouseIsOverEntityWithSize :: (DoesIO r
                                ,ModifiesComponent PictureTransform r
                                ) => Entity -> V2 Float -> Eff r Bool
getMouseIsOverEntityWithSize actor sz = do
  P vi  <- io getAbsoluteMouseLocation
  ptfrm <- getPicTransform actor >>= \case
    Nothing -> return mempty
    Just t  -> return t
  let vf = fromIntegral <$> vi
      bb = applyPicTfrmToBounds ptfrm (0,sz)
  return $ pointInBounds vf bb

buttonDown :: Script -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonDown script sz rs btn = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  if not isDown
    then do isStillOver <- getMouseIsOverEntityWithSize btn sz
            when isStillOver $ performScript btn script
            btn `setRenderer` btnRndrsUp rs
            buttonUp script sz rs btn
    else nextScript $ buttonDown script sz rs btn

buttonOver :: Script -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonOver script sz rs btn = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then if isDown then do btn `setRenderer` btnRndrsDown rs
                                   buttonDown script sz rs btn
                           else nextScript $ buttonOver script sz rs btn
            else do btn `setRenderer` btnRndrsUp rs
                    buttonUp script sz rs btn

buttonUp :: Script -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonUp script sz rs btn = do
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then do btn `setRenderer` btnRndrsOver rs
                    buttonOver script sz rs btn
            else nextScript $ buttonUp script sz rs btn

-- | Creates a fresh button.
freshButton :: (DoesIO r
               ,MakesEntities r
               ,Reads Rez r
               ,ModifiesComponent RenderIO r
               ,ModifiesComponent DeallocIO r
               ,ModifiesComponent PictureTransform r
               ,ModifiesComponent [Script] r
               ) => ButtonData -> V2 Float -> (Entity -> ScriptStep) -> Eff r Entity
freshButton btn@ButtonData{..} pos fscript = do
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
  actor `addScript` buttonUp (Script $ fscript actor) sz rs actor
  return actor
