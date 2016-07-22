{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Odin.Scripts.Status (freshStatusBar, freshStatusPrint) where

import Gelatin.SDL2
import Text.Printf
import qualified Data.IntMap as IM
import Data.Word (Word32)
import Control.Lens

import Odin.Core

updateStatus :: Entity -> Maybe FontData -> [Word32] -> Word32 -> System Script
updateStatus k mfont dts0 t0 = do
  dt       <- use (time.timeDelta)
  namez    <- use names
  tfrmz    <- use tfrms
  rs       <- use rndrs
  ds       <- use deallocs
  let dts1 = take 100 $ dt:dts0
      avg  = (realToFrac $ sum dts1 :: Double) / 100
      fps  = round $ 1 / avg * 1000

      dtstr = printf "Delta(ms):%1.2f\nFPS:%2i" avg (fps :: Int)
      nstr = printf "Names: %i" (IM.size namez)
      tstr = printf "Tfrms: %i" (IM.size tfrmz)
      rstr = printf "Rndrs: %i" (IM.size rs)
      dstr = printf "Deallocs: %i" (IM.size ds)
      namesStr = show $ IM.toList namez

      showPic :: FontData -> System ()
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
        k .# rndr r
          #. dloc c

  t1 <- if t0 >= 1000
          then do case mfont of
                    Nothing   -> io $ putStrLn $ unlines [dtstr,nstr,tstr,rstr,dstr,namesStr,[]]
                    Just font -> showPic font
                  return $ t0 - 1000
          else return $ t0 + dt
  nextScript $ updateStatus k mfont dts1 t1

freshStatusBar :: (Fresh s m, Tfrms s m, Scripts s m) => FontData -> m Entity
freshStatusBar font = do
  k <- fresh
  k .# tfrm mempty
    ## script [Script $ updateStatus k (Just font) (replicate 100 0) 0]

freshStatusPrint :: (Fresh s m, Scripts s m) => m Entity
freshStatusPrint = do
  k <- fresh
  k .# script [Script $ updateStatus k Nothing (replicate 100 0) 0]
