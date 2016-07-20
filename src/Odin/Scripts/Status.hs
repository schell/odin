{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Odin.Scripts.Status (freshStatusBar, freshStatusPrint) where

import Gelatin.SDL2
import Text.Printf
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Word (Word32)

import Odin.Core

updateStatus :: (Reads Time m
                ,Reads Rez m
                ,ModifiesComponent Name m
                ,ModifiesComponent PictureTransform m
                ,ModifiesComponent RenderIO m
                ,ModifiesComponent DeallocIO m
                ,ModifiesComponent [Script] m
                ,DoesIO m
                ) => Entity -> Maybe FontData -> [Word32] -> Word32 -> m Script
updateStatus k mfont dts0 t0 = do
  dt <- readTimeDelta
  (names   :: IntMap Name) <- get
  (tfrms   :: IntMap PictureTransform) <- get
  (rs      :: IntMap RenderIO) <- get
  (ds      :: IntMap DeallocIO) <- get
  let dts1 = take 100 $ dt:dts0
      avg  = (realToFrac $ sum dts1 :: Double) / 100
      fps  = round $ 1 / avg * 1000

      dtstr = printf "Delta(ms):%1.2f\nFPS:%2i" avg (fps :: Int)
      nstr = printf "Names: %i" (IM.size names)
      tstr = printf "Tfrms: %i" (IM.size tfrms)
      rstr = printf "Rndrs: %i" (IM.size rs)
      dstr = printf "Deallocs: %i" (IM.size ds)
      namesStr = show $ IM.toList names

      showPic :: (ModifiesComponent RenderIO m
                 ,ModifiesComponent DeallocIO m
                 ,Reads Rez m
                 ,DoesIO m
                 ) => FontData -> m ()
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

  t1 <- if t0 >= 1000
          then do case mfont of
                    Nothing   -> io $ putStrLn $ unlines [dtstr,nstr,tstr,rstr,dstr,namesStr,[]]
                    Just font -> showPic font
                  return $ t0 - 1000
          else return $ t0 + dt
  nextScript $ updateStatus k mfont dts1 t1

freshStatusBar :: (MakesEntities m
                  ,ModifiesComponent PictureTransform m
                  ,ModifiesComponent [Script] m
                  ) => FontData -> m Entity
freshStatusBar font = do
  k <- fresh
  k `setPicTransform` mempty
  k `addScript` updateStatus k (Just font) (replicate 100 0) 0
  return k

freshStatusPrint :: (MakesEntities m
                    ,ModifiesComponent [Script] m
                    ) => m Entity
freshStatusPrint = do
  k <- fresh
  k `addScript` updateStatus k Nothing (replicate 100 0) 0
  return k
