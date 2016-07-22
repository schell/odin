{-# LANGUAGE FlexibleContexts #-}
module Demos.MapCreator ( demo ) where

import SDL
import Gelatin.SDL2
import Control.Lens
import Odin.Core

setScaleToWindowSize :: (Reads Window m, Tfrms s m, DoesIO m) => Entity -> m Script
setScaleToWindowSize k = do
  win <- ask
  V2 cw ch <- io (SDL.get $ windowSize win)
  let [w,h] = map fromIntegral [cw,ch]
  tfrms.at k .= Just (PictureTransform (Transform 0 (V2 w h) 0) 1 1)
  nextScript $ setScaleToWindowSize k

demo :: FontData -> System ()
demo font = do
  k <- fresh
  k .# name "bg"
    ## tfrm mempty
    ## pictr (withColor $ rectangle 0 1 $ const $ hex 0x7f7f7fff)
    #. script [Script $ setScaleToWindowSize k]

  let str = "Please drag and drop an image to use as a tileset"
  fresh ## name "instructions"
        ## pos (V2 0 16)
        #. pictr (withLetters $ filled font 72 16 str $ solid white)

  return ()
