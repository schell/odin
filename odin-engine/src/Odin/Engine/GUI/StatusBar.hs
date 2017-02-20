{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.Engine.GUI.StatusBar where

import Text.Printf
import Data.Word (Word32)
import Gelatin

import Odin.Engine.Eff
import Odin.Engine.Slots
import Odin.Engine.GUI.Text.Internal

data StatusBar = StatusBar { statusText    :: Slot Text
                           , statusFont    :: FontDescriptor
                           , statusColor   :: V4 Float
                           , statusFrames  :: [Word32]
                           , statusCurrent :: Word32
                           }

renderStatusBar
  :: (Member IO r, ReadsRenderers r, AltersFontMap r, AltersTime r)
  => Slot StatusBar
  -> [RenderTransform2]
  -> Eff r ()
renderStatusBar s rs = do
  sb@StatusBar{..} <- unslot s
  dt         <- timeDelta <$> get
  let deltas = take 100 $ dt:statusFrames
      avg    = (realToFrac $ sum deltas :: Double) / 100
      fps    = round $ 1 / avg * 1000
      str :: String
      str = printf "Delta(ms):%1.2f FPS:%2i" avg (fps :: Int)
      elapsed = 500

  if statusCurrent >= elapsed
    then do
      reslotText statusText statusFont statusColor str
      renderText statusText rs
      reslot s sb{ statusFrames = deltas
                   , statusCurrent = statusCurrent - elapsed
                   }
  else do renderText statusText rs
          reslot s sb{ statusCurrent = statusCurrent + dt
                       , statusFrames = deltas
                       }

slotStatusBar
  :: (Member IO r, ReadsRenderers r, AltersFontMap r, Member Allocates r)
  => FontDescriptor
  -> V4 Float
  -> Eff r (Slot StatusBar)
slotStatusBar desc color = do
  txt <- slotText desc color "Status..."
  slot (StatusBar txt desc color [] 0) $ const $ return ()
