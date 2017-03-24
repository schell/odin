{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Odin.Engine.GUI.Picture where

import Odin.Engine
import Odin.Engine.Slots

slotPicture
  :: (Member IO r, Member Allocates r)
  => OdinRenderer v
  -> Picture GLuint v a -> Eff r (a, Slot Renderer2)
slotPicture b pic = do
  (a, glr)  <- io $ compilePicture b pic
  s         <- slot glr fst
  return (a, s)

slotColorPicture
  :: (Member IO r, ReadsRenderers r, Member Allocates r)
  => ColorPicture a
  -> Eff r (a, Slot Renderer2)
slotColorPicture = (v2v4Backend >>=) . flip slotPicture

slotTexturePicture
  :: (Member IO r, ReadsRenderers r, Member Allocates r)
  => TexturePicture a
  -> Eff r (a, Slot Renderer2)
slotTexturePicture = (v2v2Backend >>=) . flip slotPicture

reslotPicture
  :: (Member IO r, ReadsRenderers r)
  => Slot Renderer2
  -> OdinRenderer v
  -> Picture GLuint v a
  -> Eff r a
reslotPicture s b pic = do
  freePicture s
  (a, glr) <- io $ compilePicture b pic
  s $= glr
  return a

reslotTexturePicture
  :: (Member IO r, ReadsRenderers r)
  => Slot Renderer2 -> TexturePicture a
  -> Eff r a
reslotTexturePicture s = (v2v2Backend >>=) . flip (reslotPicture s)

reslotColorPicture
  :: (Member IO r, ReadsRenderers r)
  => Slot Renderer2
  -> ColorPicture a -> Eff r a
reslotColorPicture s = (v2v4Backend >>=) . flip (reslotPicture s)

renderPicture
  :: Member IO r
  => Slot Renderer2
  -> [RenderTransform2]
  -> Eff r ()
renderPicture s rs = fromSlotM s $ \(_,r) -> io $ r rs

freePicture
  :: Member IO r
  => Slot Renderer2
  -> Eff r ()
freePicture s = fromSlotM s $ \(c,_) -> io c
