{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Odin.Scripts.Status (freshStatusBar, freshStatusPrint) where

import Gelatin.SDL2
import Text.Printf
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

import Odin.Common
import Odin.Component

updateStatus :: (Modifies Time r
                ,ModifiesComponent Name r
                ,ModifiesComponent PictureTransform r
                ,ModifiesComponent RenderIO r
                ,ModifiesComponent DeallocIO r
                ,ModifiesComponent [Script] r
                ,Reads Rez r
                ,DoesIO r
                ) => Entity -> Maybe FontData -> [Float] -> Float -> Eff r Script
updateStatus k mfont dts0 t0 = do
  dt <- getTimeDelta
  (names   :: IntMap Name) <- get
  (tfrms   :: IntMap PictureTransform) <- get
  (rs      :: IntMap RenderIO) <- get
  (ds      :: IntMap DeallocIO) <- get
  (scripts :: IntMap [Script]) <- get
  let dts1 = take 100 $ dt:dts0
      avg  = sum dts1 / 100
      fps  = round $ 1 / avg

      dtstr = printf "Time dt:%1.2f fps:%2i" avg (fps :: Int)
      nstr = printf "Names: %i" (IM.size names)
      tstr = printf "Tfrms: %i" (IM.size tfrms)
      rstr = printf "Rndrs: %i" (IM.size rs)
      dstr = printf "Deallocs: %i" (IM.size ds)
      namesStr = show $ IM.toList names

      showPic :: (ModifiesComponent RenderIO r
                 ,ModifiesComponent DeallocIO r
                 ,Reads Rez r
                 ,DoesIO r
                 ) => FontData -> Eff r ()
      showPic font = do
        let dtpic = move (V2 0 16) $ withLetters $ filled font 128 16 dtstr $ solid white
            npic = move (V2 0 32) $ withLetters $ filled font 128 16 nstr $ solid white
            tpic = move (V2 0 48) $ withLetters $ filled font 128 16 tstr $ solid white
            rpic = move (V2 0 64) $ withLetters $ filled font 128 16 rstr $ solid white
            dpic = move (V2 0 80) $ withLetters $ filled font 128 16 dstr $ solid white
            namesPic = move (V2 0 112) $ withLetters $ filled font 128 12 namesStr $
              solid white
            pic = sequence_ [dtpic,npic,tpic,rpic,dpic,namesPic]
        dealloc k
        (c,r) <- allocPicRenderer pic
        k `setRenderer` r
        k `setDealloc` c

  t1 <- if t0 >= 1
          then do case mfont of
                    Nothing   -> io $ putStrLn $ unlines [dtstr,nstr,tstr,rstr,dstr,namesStr,[]]
                    Just font -> showPic font
                  return $ t0 - 1
          else return $ t0 + dt
  nextScript $ updateStatus k mfont dts1 t1

freshStatusBar :: (MakesEntities r
                  ,ModifiesComponent PictureTransform r
                  ,ModifiesComponent [Script] r
                  ) => FontData -> Eff r Entity
freshStatusBar font = do
  k <- fresh
  k `setPicTransform` mempty
  k `addScript` updateStatus k (Just font) (replicate 100 0) 0
  return k

freshStatusPrint :: (MakesEntities r
                    ,ModifiesComponent [Script] r
                    ) => Eff r Entity
freshStatusPrint = do
  k <- fresh
  k `addScript` updateStatus k Nothing (replicate 100 0) 0
  return k
