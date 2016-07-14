{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Odin.Scripts.Button (freshButton) where

import Linear.Affine (Point(..))
import Gelatin.SDL2
import SDL.Input.Mouse (MouseButton(..)
                       ,getAbsoluteMouseLocation
                       ,getMouseButtons
                       )
import Control.Monad (when)

import Odin.Common
import Odin.Component
import Odin.System

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 deriving (Show, Eq, Ord, Enum, Bounded)

data ButtonRndrs = ButtonRndrs { btnRndrsUp   :: RenderIO
                               , btnRndrsOver :: RenderIO
                               , btnRndrsDown :: RenderIO
                               }

textColorForButtonState :: ButtonState -> V4 Float
textColorForButtonState ButtonStateUp   = hex 0x333333FF
textColorForButtonState ButtonStateOver = V4 0.74 0.23 0.22 1
textColorForButtonState ButtonStateDown = V4 0.74 0.23 0.22 1

bgColorForButtonState :: ButtonState -> V4 Float
bgColorForButtonState ButtonStateUp   = hex 0xFFFFFFFF
bgColorForButtonState ButtonStateOver = hex 0xFFFFFFFF
bgColorForButtonState ButtonStateDown = hex 0xFFFFFFFF

bgOffsetForButtonState :: ButtonState -> V2 Float
bgOffsetForButtonState ButtonStateUp   = V2 0 0
bgOffsetForButtonState ButtonStateOver = V2 0 0
bgOffsetForButtonState ButtonStateDown = V2 2 2

applyTfrmToBounds :: Transform -> BBox -> BBox
applyTfrmToBounds t (tl,br) = pointsBounds [transformV2 t tl, transformV2 t br]

applyPicTfrmToBounds :: PictureTransform -> BBox -> BBox
applyPicTfrmToBounds (PictureTransform tfrm _ _) = applyTfrmToBounds tfrm

buttonPic :: FontData -> String -> ButtonState -> Pic
buttonPic font str btn = do
  let text = withLetters $ filled font 128 16 str $
               solid $ textColorForButtonState btn
      tsz  = pictureSize text
      txy  = V2 0 5
      pad  = V2 4 4
      sz   = tsz + 2*pad
      shxy = V2 4 4
      bgxy = bgOffsetForButtonState btn
  move shxy $ withColor $ rectangle 0 sz $ const $ V4 0 0 0 0.4
  move bgxy $ withColor $ rectangle 0 sz $ const $ bgColorForButtonState btn
  move (V2 0 16 - txy + pad + bgxy) text

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
            when isStillOver $ performScript script
            btn `setRenderer` btnRndrsUp rs
            buttonUp script sz rs btn
    else return $ Script $ buttonDown script sz rs btn

buttonOver :: Script -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonOver script sz rs btn = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then if isDown then do btn `setRenderer` btnRndrsDown rs
                                   buttonDown script sz rs btn
                           else return $ Script $ buttonOver script sz rs btn
            else do btn `setRenderer` btnRndrsUp rs
                    buttonUp script sz rs btn

buttonUp :: Script -> V2 Float -> ButtonRndrs -> Entity -> System Script
buttonUp script sz rs btn = do
  isOver <- getMouseIsOverEntityWithSize btn sz
  if isOver then do btn `setRenderer` btnRndrsOver rs
                    buttonOver script sz rs btn
            else return $ Script $ buttonUp script sz rs btn

-- | Creates a fresh button.
freshButton :: (DoesIO r
               ,MakesEntities r
               ,Reads Rez r
               ,ModifiesComponent RenderIO r
               ,ModifiesComponent DeallocIO r
               ,ModifiesComponent PictureTransform r
               ,Modifies [Script] r
               ) => FontData -> String -> V2 Float -> ScriptStep -> Eff r Entity
freshButton font str pos script = do
  let sz = pictureSize $ buttonPic font str ButtonStateUp
  up   <- allocPicRenderer $ buttonPic font str ButtonStateUp
  over <- allocPicRenderer $ buttonPic font str ButtonStateOver
  down <- allocPicRenderer $ buttonPic font str ButtonStateDown
  let dalloc = mapM_ fst [up,over,down]
      rs      = ButtonRndrs (snd up) (snd over) (snd down)
  btn <- fresh
  btn `setDealloc` dalloc
  btn `setRenderer` btnRndrsUp rs
  btn `setPicTransform` PictureTransform (Transform pos 1 0) 1 1
  addScript $ buttonUp (Script script) sz rs btn
  return btn
