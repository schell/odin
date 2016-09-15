{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.StatusBar where

import Text.Printf
import Data.Word (Word32)

import Odin.Core
import Odin.GUI.Common
import Odin.GUI.Text.Internal

data StatusBar = StatusBar { statusText :: Slot Text
                           , statusFont :: FontDescriptor
                           , statusColor:: V4 Float
                           , statusFrames :: [Word32]
                           , statusCurrent :: Word32
                           }

renderStatusBar :: (MonadIO m, Rezed s m, Fonts s m, Time s m, Resources s m)
                => Slot StatusBar -> [RenderTransform] -> m ()
renderStatusBar s rs = do
  sb@StatusBar{..} <- readSlot s
  dt         <- use (time.timeDelta)
  let deltas = take 100 $ dt:statusFrames
      avg    = (realToFrac $ sum deltas :: Double) / 100
      fps    = round $ 1 / avg * 1000
      str :: String
      str = printf "Delta(ms):%1.2f FPS:%2i" avg (fps :: Int)
      elapsed = 500

  if (statusCurrent >= elapsed)
    then do
      reallocText statusText statusFont statusColor str
      renderText statusText rs
      swapSlot s sb{ statusFrames = deltas
                   , statusCurrent = statusCurrent - elapsed
                   }
  else do renderText statusText rs
          swapSlot s sb{ statusCurrent = statusCurrent + dt
                       , statusFrames = deltas
                       }

allocStatusBar :: (MonadIO m, Rezed s m, Fonts s m, Resources s m)
               => FontDescriptor -> V4 Float -> m (Slot StatusBar)
allocStatusBar desc color = do
  txt <- allocText desc color "Status..."
  s   <- allocSlot $ StatusBar txt desc color [] 0
  registerFree $ freeStatusBar s
  return s

freeStatusBar :: MonadIO m => Slot StatusBar -> m ()
freeStatusBar s = fromSlot s statusText >>= freeText