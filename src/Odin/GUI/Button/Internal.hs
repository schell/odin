{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Button.Internal where

import Gelatin.FreeType2
import Gelatin
import Gelatin.SDL2
import Control.Lens
import Linear.Affine (Point(..))
import SDL hiding (freeCursor)
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Types as Raw
import Odin.Core
import Odin.GUI.Common
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
  { btnRndrsUp   :: GUIRenderer
  , btnRndrsOver :: GUIRenderer
  , btnRndrsDown :: GUIRenderer
  }

data Button = Button
  { btnSize    :: V2 Float
  , btnRndrs   :: ButtonRndrs
  , btnState   :: ButtonState
  }
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
getMouseIsOverBox :: UIState s m => M44 Float -> V2 Float -> m Bool
getMouseIsOverBox mv sz = do
  vi <-use (ui . mousePos)
  let vf = fromIntegral <$> vi
      bb = over both (transformV2 mv) (0,sz)
  return $ pointInBounds vf bb
--------------------------------------------------------------------------------
-- A Button's life cycle
--------------------------------------------------------------------------------
-- | Slots a new button.
slotButton :: (MonadIO m, Resources s m, CompileGraphics s m, Fonts s m)
           => Painter (ButtonData, ButtonState) m -> String
           -> m (Slot Button)
slotButton painter str = do
  -- Alloc all our renderers up front
  let dat = ButtonData str
  Painting (bounds, up  ) <- unPainter painter (dat, ButtonStateUp)
  Painting (     _, ovr ) <- unPainter painter (dat, ButtonStateOver)
  Painting (     _, down) <- unPainter painter (dat, ButtonStateDown)
  let rs = ButtonRndrs up ovr down
      size = uncurry (flip (-)) bounds
  s <- slot $ Button size rs ButtonStateUp
  registerFree $ freeButton s
  return s

-- | Reslots a button, allowing you to change its appearance.
reslotButton :: (MonadIO m, Resources s m, CompileGraphics s m, Fonts s m)
              => Slot Button -> Painter (ButtonData, ButtonState) m -> String
              -> m ()
reslotButton s painter str = do
  freeButton s
  let dat = ButtonData str
  Painting (bounds, up  ) <- unPainter painter (dat, ButtonStateUp)
  Painting (     _, ovr ) <- unPainter painter (dat, ButtonStateOver)
  Painting (     _, down) <- unPainter painter (dat, ButtonStateDown)
  let rs = ButtonRndrs up ovr down
      size = uncurry (flip (-)) bounds
  modifySlot s $ \b -> b{btnRndrs=rs
                        ,btnSize=size
                        }

freeButton :: MonadIO m => Slot Button -> m ()
freeButton s = do
  Button{..} <- unslot s
  io $ fst $ btnRndrsUp   btnRndrs
  io $ fst $ btnRndrsOver btnRndrs
  io $ fst $ btnRndrsDown btnRndrs

-- | Renders a slotted button.
renderButton :: (MonadIO m, UIState s m)
             => Slot Button -> [RenderTransform2] -> m ButtonState
renderButton s rs = do
  btn@Button{..} <- unslot s
  let mv            = affine2sModelview $ extractSpatial rs
      renderBtnUp   = io $ snd (btnRndrsUp btnRndrs) rs
      renderBtnOver = io $ snd (btnRndrsOver btnRndrs) rs
      renderBtnDown = io $ snd (btnRndrsDown btnRndrs) rs
      updateBtn st  = do
        reslot s btn{btnState = st}
        return st
  case btnState of
    ButtonStateOver -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
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
                  then do ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
                          return ButtonStateClicked
                  else return ButtonStateUp
                updateBtn st
        else do renderBtnDown
                ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
                return ButtonStateDown
    -- ButtonStateUp or ButtonStateClicked
    _   -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do renderBtnOver
                ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
                updateBtn ButtonStateOver
        else do renderBtnUp
                return ButtonStateUp
--------------------------------------------------------------------------------
-- Button properties
--------------------------------------------------------------------------------
-- | Retrieves the size of the slotted button.
sizeOfButton :: MonadIO m => Slot Button -> m (V2 Float)
sizeOfButton = flip fromSlot btnSize
