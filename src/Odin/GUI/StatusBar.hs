{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.StatusBar where

import Text.Printf
import Data.Word (Word32)

import Odin.Core.Common
import Odin.GUI.Text.Internal

data StatusBar = StatusBar { statusText :: Slot Text
                           , statusFont :: FontDescriptor
                           , statusColor:: V4 Float
                           , statusFrames :: [Word32]
                           , statusCurrent :: Word32
                           }

renderStatusBar :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m, Time s m)
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
      freeText statusText
      txt <- allocText statusFont statusColor str
      renderText txt rs
      swapSlot s sb{ statusText = txt
                   , statusFrames = deltas
                   , statusCurrent = statusCurrent - elapsed
                   }
  else do renderText statusText rs
          swapSlot s sb{ statusCurrent = statusCurrent + dt
                       , statusFrames = deltas
                       }

allocStatusBar :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m)
               => FontDescriptor -> V4 Float -> m (Slot StatusBar)
allocStatusBar desc color = do
  txt <- allocText desc color "Status..."
  allocSlot $ StatusBar txt desc color [] 0

freeStatusBar :: MonadIO m => Slot StatusBar -> m ()
freeStatusBar s = fromSlot s statusText >>= freeText

withStatusBar :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m)
               => FontDescriptor -> V4 Float -> (Slot StatusBar -> m b) -> m b
withStatusBar desc color f = do
  s <- allocStatusBar desc color
  b <- f s
  freeStatusBar s
  return b
