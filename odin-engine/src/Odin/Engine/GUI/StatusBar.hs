{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Odin.Engine.GUI.StatusBar where

import Control.Monad.IO.Class (MonadIO(..))
import           Data.Word                     (Word32)
import           Gelatin
import           Text.Printf

import           Odin.Engine
import           Odin.Engine.GUI.Text.Internal
import           Odin.Engine.Slots

data StatusBar = StatusBar { statusText    :: Slot Text
                           , statusFont    :: FontDescriptor
                           , statusColor   :: V4 Float
                           , statusFrames  :: [Word32]
                           , statusCurrent :: Word32
                           }

renderStatusBar
  :: (MonadIO m, ReadsRenderers m, Mutate FontMap m, Mutate SystemTime m)
  => Slot StatusBar
  -> [RenderTransform2]
  -> m ()
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
  :: (MonadIO m, ReadsRenderers m, Mutate FontMap m, MonadSafe m)
  => FontDescriptor
  -> V4 Float
  -> m (Slot StatusBar)
slotStatusBar desc color = do
  txt <- slotText desc color "Status..."
  slot (StatusBar txt desc color [] 0) $ const $ return ()
