{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Odin.Scripts.Status (freshStatusBar, freshStatusPrint) where

import Gelatin.SDL2
import Gelatin.FreeType2
import Text.Printf
import qualified Data.IntMap as IM
import Data.Word (Word32)
import Control.Lens

import Odin.Core

-- | TODO: Make the atlas' resources dependent upon the existance of k
updateStatus :: Entity -> Maybe Atlas -> [Word32] -> Word32 -> Evented ()
updateStatus k matlas dts0 t0 = do
  dt         <- lift $ use (time.timeDelta)
  (strLines,dts1) <- lift $ do
    --namez    <- use names
    tfrmz    <- use tfrms
    rs       <- use rndrs
    ds       <- use deallocs
    let dts1 = take 100 $ dt:dts0
        avg  = (realToFrac $ sum dts1 :: Double) / 100
        fps  = round $ 1 / avg * 1000

        dtstr = printf "Delta(ms):%1.2f\nFPS:%2i" avg (fps :: Int)
        --nstr = printf "Names: %i" (IM.size namez)
        tstr = printf "Tfrms: %i" (IM.size tfrmz)
        rstr = printf "Rndrs: %i" (IM.size rs)
        dstr = printf "Deallocs: %i" (IM.size ds)
        --namesStr = show $ IM.toList namez
    return $ (unlines [dtstr,{-nstr,-}tstr,rstr,dstr,{-namesStr,-}[]],dts1)

  let showPic :: Atlas -> System ()
      showPic atlas = do
        let picLine i str = embed $ do
              move (V2 0 $ i * 16)
              freetypePicture atlas white str
        dealloc k
        _ <- k .# texPic (mapM_ (uncurry picLine) $ zip [0..] $ lines strLines)
        return ()

  t1 <- if t0 >= 1000
          then do case matlas of
                    Nothing   -> io $ putStrLn strLines
                    Just atlas -> lift $ showPic atlas
                  return $ t0 - 1000
          else return $ t0 + dt

  next $ updateStatus k matlas dts1 t1

freshStatusBar :: (Fresh s m, Tfrms s m, Scripts s m, MonadIO m)
               => Atlas -> m Entity
freshStatusBar atlas = do
  k <- fresh
  k .# tfrm mempty
    ## script [Script $ runEventedScript $ updateStatus k (Just atlas) (replicate 100 0) 0]

freshStatusPrint :: (Fresh s m, Scripts s m) => m Entity
freshStatusPrint = do
  k <- fresh
  k .# script [Script $ runEventedScript $ updateStatus k Nothing (replicate 100 0) 0]
