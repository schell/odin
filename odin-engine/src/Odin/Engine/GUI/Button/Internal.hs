{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.Engine.GUI.Button.Internal where

import Control.Monad.IO.Class (MonadIO(..))
import Gelatin.SDL2
import SDL hiding (freeCursor)
import SDL.Raw.Enum

import Odin.Engine
import Odin.Engine.Slots
--------------------------------------------------------------------------------
-- Button Types
--------------------------------------------------------------------------------
data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 | ButtonStateClicked
                 deriving (Show, Eq, Ord, Enum, Bounded)

newtype ButtonData = ButtonData
  { btnDataStr  :: String }

data ButtonRndrs = ButtonRndrs
  { btnRndrsUp   :: Renderer2
  , btnRndrsOver :: Renderer2
  , btnRndrsDown :: Renderer2
  }

data Button = Button
  { btnSize    :: V2 Float
  , btnRndrs   :: ButtonRndrs
  , btnState   :: ButtonState
  }
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
getMouseIsOverBox :: Mutate Ui m => M44 Float -> V2 Float -> m Bool
getMouseIsOverBox mv sz = do
  vi <- getMousePosition
  let vf = fromIntegral <$> vi
      bb = both (transformV2 mv) (0,sz)
  return $ pointInBox vf bb
--------------------------------------------------------------------------------
-- Button Life Cycle
--------------------------------------------------------------------------------
setupButton
  :: Monad m
  => Painter (ButtonData, ButtonState) m
  -> String
  -> m (ButtonRndrs, V2 Float)
setupButton painter str = do
  let dat = ButtonData str
  Painting bounds up   <- unPainter painter (dat, ButtonStateUp)
  Painting      _ ovr  <- unPainter painter (dat, ButtonStateOver)
  Painting      _ down <- unPainter painter (dat, ButtonStateDown)
  return (ButtonRndrs up ovr down, uncurry (flip (-)) bounds)

-- | Slots a new button.
slotButton
  :: (MonadSafe m, ReadsRenderers m)
  => Painter (ButtonData, ButtonState) m
  -> String
  -> m (Slot Button)
slotButton painter str = do
  -- Alloc all our renderers up front
  (rs, size) <- setupButton painter str
  slot (Button size rs ButtonStateUp) freeButton

-- | Reslots a button, allowing you to change its appearance.
reslotButton
  :: (MonadIO m, ReadsRenderers m)
  => Slot Button
  -> Painter (ButtonData, ButtonState) m -> String
  -> m ()
reslotButton s painter str = do
  -- Manually free the current allocated button
  unslot s >>= liftIO . freeButton
  -- Allocate new stuff which will be free'd automatically
  (rs, size) <- setupButton painter str
  modifySlot s $ \b -> b{btnRndrs=rs
                        ,btnSize=size
                        }

freeButton :: Button -> IO ()
freeButton Button{..} = do
  fst $ btnRndrsUp   btnRndrs
  fst $ btnRndrsOver btnRndrs
  fst $ btnRndrsDown btnRndrs

-- | Renders a slotted button.
renderButton
  :: ( Mutate Ui m)
  => Slot Button -> [RenderTransform2] -> m ButtonState
renderButton s rs = do
  btn@Button{..} <- unslot s
  let mv            = affine2sModelview $ extractSpatial rs
      renderBtnUp   = liftIO $ snd (btnRndrsUp btnRndrs) rs
      renderBtnOver = liftIO $ snd (btnRndrsOver btnRndrs) rs
      renderBtnDown = liftIO $ snd (btnRndrsDown btnRndrs) rs
      updateBtn st  = do
        reslot s btn{btnState = st}
        return st
  case btnState of
    ButtonStateOver -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do setSystemCursor SDL_SYSTEM_CURSOR_HAND
                leftMouseIsDown <- queryMouseButton ButtonLeft
                if leftMouseIsDown
                  then do renderBtnDown
                          updateBtn ButtonStateDown
                  else do renderBtnOver
                          return ButtonStateOver
        else do renderBtnUp
                updateBtn ButtonStateUp
    ButtonStateDown -> do
      leftMouseIsUp <- not <$> queryMouseButton ButtonLeft
      if leftMouseIsUp
        then do renderBtnUp
                isOver <- getMouseIsOverBox mv btnSize
                st <- if isOver
                  then do setSystemCursor SDL_SYSTEM_CURSOR_HAND
                          return ButtonStateClicked
                  else return ButtonStateUp
                updateBtn st
        else do renderBtnDown
                setSystemCursor SDL_SYSTEM_CURSOR_HAND
                return ButtonStateDown
    -- ButtonStateUp or ButtonStateClicked
    _   -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do renderBtnOver
                setSystemCursor SDL_SYSTEM_CURSOR_HAND
                updateBtn ButtonStateOver
        else do renderBtnUp
                return ButtonStateUp
--------------------------------------------------------------------------------
-- Button properties
--------------------------------------------------------------------------------
-- | Retrieves the size of the slotted button.
sizeOfButton :: MonadIO m => Slot Button -> m (V2 Float)
sizeOfButton = flip fromSlot btnSize
