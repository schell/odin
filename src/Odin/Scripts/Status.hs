{-# LANGUAGE FlexibleContexts #-}
module Odin.Scripts.Status (freshStatusBar) where

import Gelatin.SDL2
import Text.Printf

import Odin.Common
import Odin.Component

updateStatus :: (Modifies Time r
                ,ModifiesComponent RenderIO r
                ,ModifiesComponent DeallocIO r
                ,Reads Rez r
                ,DoesIO r
                ) => Entity -> FontData -> [Float] -> Eff r Script
updateStatus k font dts0 = do
  dealloc k
  dt <- getTimeDelta
  let dts1 = take 100 $ dt:dts0
      avg  = sum dts1 / 100
      fps  = round $ 1 / avg
      str  = printf "dt:%1.2f fps:%2i" avg (fps :: Int)
      pic  = move (V2 0 16) $ withLetters $ filled font 128 16 str $ solid white
  (c,r) <- allocPicRenderer pic
  k `setRenderer` r
  k `setDealloc` c
  nextScript $ updateStatus k font dts1

freshStatusBar :: (MakesEntities r
                  ,ModifiesComponent PictureTransform r
                  ,Modifies [Script] r
                  ) => FontData -> Eff r Entity
freshStatusBar font = do
  k <- fresh
  k `setPicTransform` mempty
  addScript $ updateStatus k font $ replicate 100 0
  return k
