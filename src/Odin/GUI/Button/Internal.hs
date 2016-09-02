{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Button.Internal where

import Gelatin.FreeType2
import Gelatin
import Gelatin.SDL2
import Control.Lens
import Linear.Affine (Point(..))
import SDL
import Odin.Core
--------------------------------------------------------------------------------
-- Button Types
--------------------------------------------------------------------------------
data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 | ButtonStateClicked
                 deriving (Show, Eq, Ord, Enum, Bounded)

data ButtonData = ButtonData
  { btnDataAtlas     :: Atlas
  , btnDataStr       :: String
  }

data ButtonRndrs = ButtonRndrs
  { btnRndrsUp   :: RenderIO
  , btnRndrsOver :: RenderIO
  , btnRndrsDown :: RenderIO
  }

data Button = Button
  { btnUid     :: Int
  , btnSize    :: V2 Float
  , btnRndrs   :: ButtonRndrs
  , btnDealloc :: IO ()
  , btnState   :: ButtonState
  }
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
getMouseIsOverBox :: MonadIO m => M44 Float -> V2 Float -> m Bool
getMouseIsOverBox mv sz = do
  P vi  <- io getAbsoluteMouseLocation
  let vf = fromIntegral <$> vi
      bb = over both (transformV2' mv) (0,sz)
  return $ pointInBounds vf bb
--------------------------------------------------------------------------------
-- A Button's life cycle
--------------------------------------------------------------------------------
-- Allocs a new button.
allocButton :: (MonadIO m, Fresh s m, Rezed s m)
            => Atlas -> String -> Painter (ButtonData, ButtonState) m
            -> m (Slot Button)
allocButton atlas str painter = do
  -- Alloc all our renderers up front
  let dat = ButtonData atlas str
  k    <- fresh
  sz   <- runPainterSize painter (dat, ButtonStateUp)
  up   <- compilePainter painter (dat, ButtonStateUp)
  ovr  <- compilePainter painter (dat, ButtonStateOver)
  down <- compilePainter painter (dat, ButtonStateDown)
  let d  = mapM_ fst [up,ovr,down]
      rs = ButtonRndrs (snd up) (snd ovr) (snd down)
  allocSlot $ Button k sz rs d ButtonStateUp

freeButton :: MonadIO m => Slot Button -> m ()
freeButton s = fromSlot s btnDealloc >>= io

withButton :: (MonadIO m, Fresh s m, Rezed s m)
           => Atlas -> String -> Painter (ButtonData, ButtonState) m
           -> (Slot Button -> m b)
           -> m b
withButton atlas str painter f = do
  btn <- allocButton atlas str painter
  a <- f btn
  freeButton btn
  return a

renderButton :: MonadIO m => Slot Button -> [RenderTransform] -> m ButtonState
renderButton s rs = do
  btn@Button{..} <- readSlot s
  let t             = rendersToPictureTransform rs
      mv            = ptfrmMV t
      renderBtnUp   = io $ btnRndrsUp btnRndrs t
      renderBtnOver = io $ btnRndrsOver btnRndrs t
      renderBtnDown = io $ btnRndrsDown btnRndrs t
      updateBtn st  = swapSlot s btn{btnState = st} >> return st
  case btnState of
    ButtonStateOver -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do leftMouseIsDown <- ($ ButtonLeft) <$> io getMouseButtons
                if leftMouseIsDown
                  then do renderBtnDown
                          updateBtn ButtonStateDown
                  else do renderBtnOver
                          return ButtonStateOver
        else do renderBtnUp
                updateBtn ButtonStateUp
    ButtonStateDown -> do
      leftMouseIsUp <- (not . ($ ButtonLeft)) <$> io getMouseButtons
      if leftMouseIsUp
        then do renderBtnUp
                isOver <- getMouseIsOverBox mv btnSize
                updateBtn $ if isOver then ButtonStateClicked else ButtonStateUp
        else do renderBtnDown
                return ButtonStateDown
    -- ButtonStateUp or ButtonStateClicked
    _   -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do renderBtnOver
                updateBtn ButtonStateOver
        else do renderBtnUp
                return ButtonStateUp
--------------------------------------------------------------------------------
-- Button properties
--------------------------------------------------------------------------------
sizeOfButton :: MonadIO m => Slot Button -> m (V2 Float)
sizeOfButton = flip fromSlot btnSize
