{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.Engine.GUI.Button.Internal where

--import Gelatin.FreeType2
--import Gelatin hiding (both)
import Gelatin.SDL2 hiding (both)
import Control.Lens
--import Control.Monad.Trans
--import Linear.Affine (Point(..))
import SDL hiding (freeCursor)
--import qualified SDL.Raw.Event as Raw
import SDL.Raw.Enum
import Odin.Engine.Eff
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
getMouseIsOverBox :: AltersUI r => M44 Float -> V2 Float -> Eff r Bool
getMouseIsOverBox mv sz = do
  vi <- getMousePosition
  let vf = fromIntegral <$> vi
      bb = over both (transformV2 mv) (0,sz)
  return $ pointInBox vf bb
--------------------------------------------------------------------------------
-- Button Life Cycle
--------------------------------------------------------------------------------
setupButton
  :: Painter (ButtonData, ButtonState) r
  -> String
  -> Eff r (ButtonRndrs, V2 Float)
setupButton painter str = do
  let dat = ButtonData str
  Painting (bounds, up  ) <- unPainter painter (dat, ButtonStateUp)
  Painting (     _, ovr ) <- unPainter painter (dat, ButtonStateOver)
  Painting (     _, down) <- unPainter painter (dat, ButtonStateDown)
  return (ButtonRndrs up ovr down, uncurry (flip (-)) bounds)

-- | Slots a new button.
slotButton
  :: (Member IO r, Member Allocates r, ReadsRenderers r)
  => Painter (ButtonData, ButtonState) r
  -> String
  -> Eff r (Slot Button)
slotButton painter str = do
  -- Alloc all our renderers up front
  (rs, size) <- setupButton painter str
  slot (Button size rs ButtonStateUp) freeButton

-- | Reslots a button, allowing you to change its appearance.
reslotButton
  :: (Member IO r, ReadsRenderers r)
  => Slot Button
  -> Painter (ButtonData, ButtonState) r -> String
  -> Eff r ()
reslotButton s painter str = do
  -- Manually free the current allocated button
  unslot s >>= io . freeButton
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
  :: (Member IO r, AltersUI r)
  => Slot Button -> [RenderTransform2] -> Eff r ButtonState
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
sizeOfButton :: Member IO r => Slot Button -> Eff r (V2 Float)
sizeOfButton = flip fromSlot btnSize
